
;; title: marketplace


(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-listing-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-wrong-status (err u104))
(define-constant err-insufficient-payment (err u105))
(define-constant err-already-purchased (err u106))
(define-constant err-not-buyer (err u107))
(define-constant err-not-seller (err u108))
(define-constant err-not-in-escrow (err u109))
(define-constant err-dispute-exists (err u110))

(define-data-var fee-percentage uint u25)
(define-data-var next-listing-id uint u1)

(define-map listings
  { listing-id: uint }
  {
    seller: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    price: uint,
    status: (string-ascii 20),
    buyer: (optional principal),
    escrow-amount: uint,
    created-at: uint,
    expires-at: uint
  }
)

(define-map disputes
  { listing-id: uint }
  {
    initiated-by: principal,
    reason: (string-ascii 500),
    resolved: bool,
    resolution: (optional (string-ascii 100))
  }
)

(define-read-only (get-listing (listing-id uint))
  (map-get? listings { listing-id: listing-id })
)

(define-read-only (get-dispute (listing-id uint))
  (map-get? disputes { listing-id: listing-id })
)

(define-read-only (get-fee-percentage)
  (var-get fee-percentage)
)

(define-read-only (calculate-fee (amount uint))
  (/ (* amount (var-get fee-percentage)) u1000)
)

(define-public (set-fee-percentage (new-fee-percentage uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee-percentage u100) (err u111))
    (ok (var-set fee-percentage new-fee-percentage))
  )
)

(define-public (create-listing (title (string-ascii 100)) (description (string-ascii 500)) (price uint))
  (let
    (
      (listing-id (var-get next-listing-id))
      (stacks-block-heights stacks-block-height)
    )
    (asserts! (> price u0) (err u112))
    (map-insert listings
      { listing-id: listing-id }
      {
        seller: tx-sender,
        title: title,
        description: description,
        price: price,
        status: "active",
        buyer: none,
        escrow-amount: u0,
        created-at: stacks-block-height,
        expires-at: u0
      }
    )
    (var-set next-listing-id (+ listing-id u1))
    (ok listing-id)
  )
)

(define-public (update-listing (listing-id uint) (title (string-ascii 100)) (description (string-ascii 500)) (price uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
    )
    (asserts! (is-eq (get seller listing) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status listing) "active") err-wrong-status)
    (asserts! (> price u0) (err u112))
    
    (map-set listings
      { listing-id: listing-id }
      (merge listing {
        title: title,
        description: description,
        price: price
      })
    )
    (ok true)
  )
)

(define-public (cancel-listing (listing-id uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
    )
    (asserts! (is-eq (get seller listing) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status listing) "active") err-wrong-status)
    
    (map-set listings
      { listing-id: listing-id }
      (merge listing {
        status: "cancelled"
      })
    )
    (ok true)
  )
)

(define-public (purchase-listing (listing-id uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (price (get price listing))
    )
    (asserts! (is-eq (get status listing) "active") err-wrong-status)
    (asserts! (not (is-eq (get seller listing) tx-sender)) (err u113))
    
    (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
    
    (map-set listings
      { listing-id: listing-id }
      (merge listing {
        status: "in-escrow",
        buyer: (some tx-sender),
        escrow-amount: price
      })
    )
    (ok true)
  )
)

(define-public (confirm-delivery (listing-id uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (price (get price listing))
      (seller (get seller listing))
      (fee (calculate-fee price))
      (seller-amount (- price fee))
    )
    (asserts! (is-some (get buyer listing)) err-not-found)
    (asserts! (is-eq (some tx-sender) (get buyer listing)) err-not-buyer)
    (asserts! (is-eq (get status listing) "in-escrow") err-not-in-escrow)
    
    (try! (as-contract (stx-transfer? seller-amount (as-contract tx-sender) seller)))
    (try! (as-contract (stx-transfer? fee (as-contract tx-sender) contract-owner)))
    
    (map-set listings
      { listing-id: listing-id }
      (merge listing {
        status: "completed",
        escrow-amount: u0
      })
    )
    (ok true)
  )
)

(define-public (refund-buyer (listing-id uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (escrow-amount (get escrow-amount listing))
    )
    (asserts! (is-eq (get seller listing) tx-sender) err-not-seller)
    (asserts! (is-eq (get status listing) "in-escrow") err-not-in-escrow)
    (asserts! (is-some (get buyer listing)) err-not-found)
    
    (try! (as-contract (stx-transfer? escrow-amount (as-contract tx-sender) (unwrap! (get buyer listing) err-not-found))))
    
    (map-set listings
      { listing-id: listing-id }
      (merge listing {
        status: "refunded",
        escrow-amount: u0
      })
    )
    (ok true)
  )
)

(define-public (open-dispute (listing-id uint) (reason (string-ascii 500)))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
    )
    (asserts! (is-eq (get status listing) "in-escrow") err-not-in-escrow)
    (asserts! (or (is-eq tx-sender (get seller listing)) (is-eq (some tx-sender) (get buyer listing))) err-unauthorized)
    (asserts! (is-none (get-dispute listing-id)) err-dispute-exists)
    
    (map-insert disputes
      { listing-id: listing-id }
      {
        initiated-by: tx-sender,
        reason: reason,
        resolved: false,
        resolution: none
      }
    )
    
    (map-set listings
      { listing-id: listing-id }
      (merge listing {
        status: "disputed"
      })
    )
    (ok true)
  )
)

(define-public (resolve-dispute (listing-id uint) (resolution (string-ascii 100)) (to-seller bool))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (dispute (unwrap! (get-dispute listing-id) err-not-found))
      (escrow-amount (get escrow-amount listing))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (is-eq (get status listing) "disputed") err-wrong-status)
    
    (if to-seller
      (try! (as-contract (stx-transfer? escrow-amount (as-contract tx-sender) (get seller listing))))
      (try! (as-contract (stx-transfer? escrow-amount (as-contract tx-sender) (unwrap! (get buyer listing) err-not-found))))
    )
    
    (map-set disputes
      { listing-id: listing-id }
      (merge dispute {
        resolved: true,
        resolution: (some resolution)
      })
    )
    
    (map-set listings
      { listing-id: listing-id }
      (merge listing {
        status: (if to-seller "resolved-to-seller" "resolved-to-buyer"),
        escrow-amount: u0
      })
    )
    (ok true)
  )
)


(define-map categories 
  { category-id: uint }
  { name: (string-ascii 50) }
)

(define-map listing-categories
  { listing-id: uint }
  { category-id: uint }
)

(define-data-var next-category-id uint u1)

(define-public (create-category (name (string-ascii 50)))
  (let
    ((category-id (var-get next-category-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-insert categories
      { category-id: category-id }
      { name: name }
    )
    (var-set next-category-id (+ category-id u1))
    (ok category-id)
  )
)

(define-public (set-listing-category (listing-id uint) (category-id uint))
  (let
    ((listing (unwrap! (get-listing listing-id) err-not-found)))
    (asserts! (is-eq (get seller listing) tx-sender) err-unauthorized)
    (map-set listing-categories
      { listing-id: listing-id }
      { category-id: category-id }
    )
    (ok true)
  )
)

;; (define-read-only (get-listings-by-category (category-id uint))
;;   (filter list-in-category (map-to-list listings))
;; )


(define-constant listing-duration u10000)

(define-public (create-listing-with-duration 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (price uint)
    (duration uint)
  )
  (let
    (
      (listing-id (var-get next-listing-id))
      (current-height stacks-block-height)
      (expiry (+ current-height duration))
    )
    (asserts! (> price u0) (err u112))
    (asserts! (>= duration u1) (err u120))
    (map-insert listings
      { listing-id: listing-id }
      {
        seller: tx-sender,
        title: title,
        description: description,
        price: price,
        status: "active",
        buyer: none,
        escrow-amount: u0,
        created-at: current-height,
        expires-at: expiry
      }
    )
    (var-set next-listing-id (+ listing-id u1))
    (ok listing-id)
  )
)

(define-read-only (is-listing-expired (listing-id uint))
  (match (get-listing listing-id)
    listing (> stacks-block-height (get expires-at listing))
    false
  )
)