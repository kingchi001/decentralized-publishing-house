;; Decentralized Publishing House Smart Contract
;; A platform for authors to publish, sell, and manage digital content with automated royalties

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-percentage (err u105))
(define-constant err-not-for-sale (err u106))

;; Data Variables
(define-data-var next-work-id uint u1)
(define-data-var platform-fee-percentage uint u250) ;; 2.5% in basis points

;; Data Maps
(define-map works
  { work-id: uint }
  {
    title: (string-ascii 100),
    author: principal,
    content-hash: (string-ascii 64),
    price: uint,
    royalty-percentage: uint,
    total-sales: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map work-ownership
  { work-id: uint, owner: principal }
  { percentage: uint }
)

(define-map author-earnings
  { author: principal }
  { total-earned: uint }
)

(define-map platform-earnings
  principal
  { total-earned: uint }
)

(define-map work-reviews
  { work-id: uint, reviewer: principal }
  {
    rating: uint,
    review: (string-utf8 500),
    timestamp: uint
  }
)

(define-map work-sales
  { work-id: uint, buyer: principal }
  {
    purchase-price: uint,
    timestamp: uint
  }
)

;; Read-only functions
(define-read-only (get-work (work-id uint))
  (map-get? works { work-id: work-id })
)

(define-read-only (get-work-ownership (work-id uint) (owner principal))
  (map-get? work-ownership { work-id: work-id, owner: owner })
)

(define-read-only (get-author-earnings (author principal))
  (default-to { total-earned: u0 } (map-get? author-earnings { author: author }))
)

(define-read-only (get-platform-earnings)
  (default-to { total-earned: u0 } (map-get? platform-earnings contract-owner))
)

(define-read-only (get-work-review (work-id uint) (reviewer principal))
  (map-get? work-reviews { work-id: work-id, reviewer: reviewer })
)

(define-read-only (get-work-sale (work-id uint) (buyer principal))
  (map-get? work-sales { work-id: work-id, buyer: buyer })
)

(define-read-only (has-purchased (work-id uint) (buyer principal))
  (is-some (map-get? work-sales { work-id: work-id, buyer: buyer }))
)

(define-read-only (get-next-work-id)
  (var-get next-work-id)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee-percentage)
)

;; Private functions
(define-private (calculate-platform-fee (amount uint))
  (/ (* amount (var-get platform-fee-percentage)) u10000)
)

(define-private (calculate-author-royalty (amount uint) (royalty-percentage uint))
  (/ (* amount royalty-percentage) u10000)
)

;; Public functions
(define-public (publish-work 
  (title (string-ascii 100))
  (content-hash (string-ascii 64))
  (price uint)
  (royalty-percentage uint))
  (let
    (
      (work-id (var-get next-work-id))
      (author tx-sender)
    )
    (asserts! (<= royalty-percentage u10000) err-invalid-percentage)
    (asserts! (is-none (map-get? works { work-id: work-id })) err-already-exists)
    
    ;; Create the work
    (map-set works
      { work-id: work-id }
      {
        title: title,
        author: author,
        content-hash: content-hash,
        price: price,
        royalty-percentage: royalty-percentage,
        total-sales: u0,
        is-active: true,
        created-at: block-height
      }
    )
    
    ;; Set initial ownership to 100% for author
    (map-set work-ownership
      { work-id: work-id, owner: author }
      { percentage: u10000 }
    )
    
    ;; Increment work ID counter
    (var-set next-work-id (+ work-id u1))
    
    (ok work-id)
  )
)

(define-public (purchase-work (work-id uint))
  (let
    (
      (work-data (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (buyer tx-sender)
      (author (get author work-data))
      (price (get price work-data))
      (royalty-percentage (get royalty-percentage work-data))
      (platform-fee (calculate-platform-fee price))
      (author-royalty (calculate-author-royalty price royalty-percentage))
      (author-payment (- price platform-fee))
    )
    
    (asserts! (get is-active work-data) err-not-for-sale)
    (asserts! (> price u0) err-not-for-sale)
    (asserts! (not (has-purchased work-id buyer)) err-already-exists)
    
    ;; Transfer payment to author
    (try! (stx-transfer? author-payment buyer author))
    
    ;; Transfer platform fee to contract owner
    (try! (stx-transfer? platform-fee buyer contract-owner))
    
    ;; Record the sale
    (map-set work-sales
      { work-id: work-id, buyer: buyer }
      {
        purchase-price: price,
        timestamp: block-height
      }
    )
    
    ;; Update work sales count
    (map-set works
      { work-id: work-id }
      (merge work-data { total-sales: (+ (get total-sales work-data) u1) })
    )
    
    ;; Update author earnings
    (let
      (
        (current-earnings (get total-earned (get-author-earnings author)))
      )
      (map-set author-earnings
        { author: author }
        { total-earned: (+ current-earnings author-payment) }
      )
    )
    
    ;; Update platform earnings
    (let
      (
        (current-platform-earnings (get total-earned (get-platform-earnings)))
      )
      (map-set platform-earnings
        contract-owner
        { total-earned: (+ current-platform-earnings platform-fee) }
      )
    )
    
    (ok true)
  )
)

(define-public (add-review (work-id uint) (rating uint) (review (string-utf8 500)))
  (let
    (
      (work-data (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (reviewer tx-sender)
    )
    
    (asserts! (<= rating u5) err-invalid-percentage)
    (asserts! (>= rating u1) err-invalid-percentage)
    (asserts! (has-purchased work-id reviewer) err-unauthorized)
    
    (map-set work-reviews
      { work-id: work-id, reviewer: reviewer }
      {
        rating: rating,
        review: review,
        timestamp: block-height
      }
    )
    
    (ok true)
  )
)

(define-public (update-work-price (work-id uint) (new-price uint))
  (let
    (
      (work-data (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (author (get author work-data))
    )
    
    (asserts! (is-eq tx-sender author) err-unauthorized)
    
    (map-set works
      { work-id: work-id }
      (merge work-data { price: new-price })
    )
    
    (ok true)
  )
)

(define-public (toggle-work-status (work-id uint))
  (let
    (
      (work-data (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (author (get author work-data))
    )
    
    (asserts! (is-eq tx-sender author) err-unauthorized)
    
    (map-set works
      { work-id: work-id }
      (merge work-data { is-active: (not (get is-active work-data)) })
    )
    
    (ok true)
  )
)

(define-public (transfer-ownership 
  (work-id uint) 
  (new-owner principal) 
  (percentage uint))
  (let
    (
      (work-data (unwrap! (map-get? works { work-id: work-id }) err-not-found))
      (current-owner tx-sender)
      (current-ownership (unwrap! (map-get? work-ownership 
        { work-id: work-id, owner: current-owner }) err-unauthorized))
    )
    
    (asserts! (<= percentage (get percentage current-ownership)) err-invalid-percentage)
    (asserts! (> percentage u0) err-invalid-percentage)
    
    ;; Update current owner's percentage
    (map-set work-ownership
      { work-id: work-id, owner: current-owner }
      { percentage: (- (get percentage current-ownership) percentage) }
    )
    
    ;; Set new owner's percentage
    (let
      (
        (existing-ownership (default-to { percentage: u0 } 
          (map-get? work-ownership { work-id: work-id, owner: new-owner })))
      )
      (map-set work-ownership
        { work-id: work-id, owner: new-owner }
        { percentage: (+ (get percentage existing-ownership) percentage) }
      )
    )
    
    (ok true)
  )
)

;; Admin functions
(define-public (set-platform-fee (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-percentage u1000) err-invalid-percentage) ;; Max 10%
    (var-set platform-fee-percentage new-fee-percentage)
    (ok true)
  )
)