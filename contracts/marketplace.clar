
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

(define-constant err-already-rated (err u203))
(define-constant err-invalid-rating (err u204))
(define-constant err-self-rating (err u205))

(define-map user-reputation
  { user: principal }
  {
    total-rating: uint,
    rating-count: uint,
    completed-sales: uint,
    completed-purchases: uint
  }
)

(define-map transaction-ratings
  { listing-id: uint, rater: principal }
  {
    rating: uint,
    comment: (string-ascii 200),
    rated-user: principal,
    created-at: uint
  }
)

(define-map auctions
  { listing-id: uint }
  {
    starting-price: uint,
    current-bid: uint,
    highest-bidder: (optional principal),
    auction-end: uint,
    bid-increment: uint,
    reserve-price: uint,
    auction-type: (string-ascii 20)
  }
)

(define-map auction-bids
  { listing-id: uint, bidder: principal }
  {
    bid-amount: uint,
    bid-time: uint,
    refunded: bool
  }
)

(define-read-only (get-user-reputation (user principal))
  (default-to 
    { total-rating: u0, rating-count: u0, completed-sales: u0, completed-purchases: u0 }
    (map-get? user-reputation { user: user })
  )
)

(define-read-only (get-average-rating (user principal))
  (let
    ((reputation (get-user-reputation user)))
    (if (> (get rating-count reputation) u0)
      (/ (get total-rating reputation) (get rating-count reputation))
      u0
    )
  )
)

(define-read-only (get-transaction-rating (listing-id uint) (rater principal))
  (map-get? transaction-ratings { listing-id: listing-id, rater: rater })
)

(define-public (rate-user (listing-id uint) (rated-user principal) (rating uint) (comment (string-ascii 200)))
  (let
    ((existing-rating (get-transaction-rating listing-id tx-sender))
     (current-reputation (get-user-reputation rated-user)))
    
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-rating)
    (asserts! (not (is-eq tx-sender rated-user)) err-self-rating)
    (asserts! (is-none existing-rating) err-already-rated)
    
    (map-insert transaction-ratings
      { listing-id: listing-id, rater: tx-sender }
      {
        rating: rating,
        comment: comment,
        rated-user: rated-user,
        created-at: stacks-block-height
      }
    )
    
    (map-set user-reputation
      { user: rated-user }
      {
        total-rating: (+ (get total-rating current-reputation) rating),
        rating-count: (+ (get rating-count current-reputation) u1),
        completed-sales: (get completed-sales current-reputation),
        completed-purchases: (get completed-purchases current-reputation)
      }
    )
    (ok true)
  )
)

(define-public (update-transaction-count (user principal) (is-seller bool))
  (let
    ((current-reputation (get-user-reputation user)))
    (map-set user-reputation
      { user: user }
      (if is-seller
        (merge current-reputation { completed-sales: (+ (get completed-sales current-reputation) u1) })
        (merge current-reputation { completed-purchases: (+ (get completed-purchases current-reputation) u1) })
      )
    )
    (ok true)
  )
)

(define-read-only (get-user-stats (user principal))
  (let
    ((reputation (get-user-reputation user)))
    {
      average-rating: (get-average-rating user),
      total-transactions: (+ (get completed-sales reputation) (get completed-purchases reputation)),
      completed-sales: (get completed-sales reputation),
      completed-purchases: (get completed-purchases reputation),
      rating-count: (get rating-count reputation)
    }
  )
)

(define-read-only (is-trusted-user (user principal))
  (let
    ((stats (get-user-stats user)))
    (and 
      (>= (get rating-count stats) u5)
      (>= (get average-rating stats) u4)
      (>= (get total-transactions stats) u3)
    )
  )
)

(define-constant err-invalid-bulk-data (err u114))
(define-constant max-bulk-listings u10)

(define-constant err-auction-ended (err u115))
(define-constant err-auction-not-ended (err u116))
(define-constant err-bid-too-low (err u117))
(define-constant err-invalid-auction-duration (err u118))
(define-constant err-not-highest-bidder (err u119))

(define-public (create-bulk-listings (listings-data (list 10 {title: (string-ascii 100), description: (string-ascii 500), price: uint})))
  (let
    ((data-length (len listings-data)))
    (asserts! (> data-length u0) err-invalid-bulk-data)
    (asserts! (<= data-length max-bulk-listings) err-invalid-bulk-data)
    
    (ok (map create-single-listing-from-bulk listings-data))
  )
)

(define-private (create-single-listing-from-bulk (listing-data {title: (string-ascii 100), description: (string-ascii 500), price: uint}))
  (let
    ((listing-id (var-get next-listing-id))
     (title (get title listing-data))
     (description (get description listing-data))
     (price (get price listing-data)))
    
    (if (> price u0)
      (begin
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
        listing-id
      )
      u0
    )
  )
)

(define-public (bulk-update-listing-status (listing-ids (list 10 uint)) (new-status (string-ascii 20)))
  (begin
    (asserts! (> (len listing-ids) u0) err-invalid-bulk-data)
    (asserts! (<= (len listing-ids) max-bulk-listings) err-invalid-bulk-data)
    
    (ok (fold update-status-fold listing-ids (list)))
  )
)

(define-private (update-status-fold (listing-id uint) (acc (list 10 bool)))
  (let
    ((listing (unwrap! (get-listing listing-id) (list false)))
     (result 
       (if (and (is-eq (get seller listing) tx-sender) (is-eq (get status listing) "active"))
         (begin
           (map-set listings
             { listing-id: listing-id }
             (merge listing { status: "cancelled" })
           )
           true
         )
         false
       )
     ))
    (unwrap! (as-max-len? (append acc result) u10) acc)
  )
)

(define-read-only (get-auction (listing-id uint))
  (map-get? auctions { listing-id: listing-id })
)

(define-read-only (get-auction-bid (listing-id uint) (bidder principal))
  (map-get? auction-bids { listing-id: listing-id, bidder: bidder })
)

(define-read-only (is-auction-active (listing-id uint))
  (match (get-auction listing-id)
    auction (< stacks-block-height (get auction-end auction))
    false
  )
)

(define-public (create-auction-listing 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (starting-price uint)
    (reserve-price uint)
    (bid-increment uint)
    (duration uint)
  )
  (let
    (
      (listing-id (var-get next-listing-id))
      (current-height stacks-block-height)
      (auction-end (+ current-height duration))
    )
    (asserts! (> starting-price u0) (err u112))
    (asserts! (>= reserve-price starting-price) (err u117))
    (asserts! (> bid-increment u0) (err u117))
    (asserts! (>= duration u144) err-invalid-auction-duration)
    
    (map-insert listings
      { listing-id: listing-id }
      {
        seller: tx-sender,
        title: title,
        description: description,
        price: starting-price,
        status: "auction",
        buyer: none,
        escrow-amount: u0,
        created-at: current-height,
        expires-at: auction-end
      }
    )
    
    (map-insert auctions
      { listing-id: listing-id }
      {
        starting-price: starting-price,
        current-bid: u0,
        highest-bidder: none,
        auction-end: auction-end,
        bid-increment: bid-increment,
        reserve-price: reserve-price,
        auction-type: "standard"
      }
    )
    
    (var-set next-listing-id (+ listing-id u1))
    (ok listing-id)
  )
)

(define-public (place-bid (listing-id uint) (bid-amount uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (auction (unwrap! (get-auction listing-id) err-not-found))
      (current-highest-bid (get current-bid auction))
      (required-bid (+ current-highest-bid (get bid-increment auction)))
      (previous-bidder (get highest-bidder auction))
    )
    (asserts! (is-eq (get status listing) "auction") err-wrong-status)
    (asserts! (is-auction-active listing-id) err-auction-ended)
    (asserts! (not (is-eq tx-sender (get seller listing))) err-unauthorized)
    (asserts! (>= bid-amount required-bid) err-bid-too-low)
    
    (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
    
    (match previous-bidder
      prev-bidder (try! (as-contract (stx-transfer? current-highest-bid (as-contract tx-sender) prev-bidder)))
      true
    )
    
    (map-set auction-bids
      { listing-id: listing-id, bidder: tx-sender }
      {
        bid-amount: bid-amount,
        bid-time: stacks-block-height,
        refunded: false
      }
    )
    
    (map-set auctions
      { listing-id: listing-id }
      (merge auction {
        current-bid: bid-amount,
        highest-bidder: (some tx-sender)
      })
    )
    
    (ok true)
  )
)

(define-public (finalize-auction (listing-id uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (auction (unwrap! (get-auction listing-id) err-not-found))
      (highest-bidder (get highest-bidder auction))
      (final-bid (get current-bid auction))
      (reserve-price (get reserve-price auction))
      (seller (get seller listing))
      (fee (calculate-fee final-bid))
      (seller-amount (- final-bid fee))
    )
    (asserts! (is-eq (get status listing) "auction") err-wrong-status)
    (asserts! (not (is-auction-active listing-id)) err-auction-not-ended)
    
    (if (and (is-some highest-bidder) (>= final-bid reserve-price))
      (begin
        (try! (as-contract (stx-transfer? seller-amount (as-contract tx-sender) seller)))
        (try! (as-contract (stx-transfer? fee (as-contract tx-sender) contract-owner)))
        
        (map-set listings
          { listing-id: listing-id }
          (merge listing {
            status: "sold",
            buyer: highest-bidder,
            escrow-amount: u0
          })
        )
        (ok "sold")
      )
      (begin
        (match highest-bidder
          winner (try! (as-contract (stx-transfer? final-bid (as-contract tx-sender) winner)))
          true
        )
        
        (map-set listings
          { listing-id: listing-id }
          (merge listing {
            status: "unsold",
            buyer: none,
            escrow-amount: u0
          })
        )
        (ok "unsold")
      )
    )
  )
)

(define-public (extend-auction (listing-id uint) (additional-blocks uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (auction (unwrap! (get-auction listing-id) err-not-found))
      (new-end (+ (get auction-end auction) additional-blocks))
    )
    (asserts! (is-eq (get seller listing) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status listing) "auction") err-wrong-status)
    (asserts! (is-auction-active listing-id) err-auction-ended)
    (asserts! (> additional-blocks u0) (err u112))
    (asserts! (<= additional-blocks u1008) err-invalid-auction-duration)
    
    (map-set auctions
      { listing-id: listing-id }
      (merge auction { auction-end: new-end })
    )
    
    (map-set listings
      { listing-id: listing-id }
      (merge listing { expires-at: new-end })
    )
    
    (ok true)
  )
)





