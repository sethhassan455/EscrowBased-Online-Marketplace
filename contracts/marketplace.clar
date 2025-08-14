
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
    
    ;; Use tier-based fee calculation instead of fixed fee
    (let ((tier-fee (calculate-tier-fee price seller))
          (tier-seller-amount (- price tier-fee)))
      (try! (as-contract (stx-transfer? tier-seller-amount (as-contract tx-sender) seller)))
      (try! (as-contract (stx-transfer? tier-fee (as-contract tx-sender) contract-owner)))
      (unwrap-panic (update-seller-volume seller price))
    )
    
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
(define-constant err-tier-not-eligible (err u120))
(define-constant err-volume-insufficient (err u121))
(define-constant err-tier-already-max (err u122))
(define-constant err-invalid-tier (err u123))

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

;; Seller tier system constants and data structures
(define-constant tier-bronze u1)
(define-constant tier-silver u2)
(define-constant tier-gold u3)
(define-constant tier-platinum u4)

;; Tier requirements (sales volume, rating, transaction count)
(define-constant bronze-min-sales u0)
(define-constant silver-min-sales u10000)
(define-constant gold-min-sales u50000)
(define-constant platinum-min-sales u200000)

(define-constant silver-min-transactions u20)
(define-constant gold-min-transactions u100)
(define-constant platinum-min-transactions u500)

(define-constant silver-min-rating u40)
(define-constant gold-min-rating u45)
(define-constant platinum-min-rating u48)

;; Commission rates per tier (basis points: 1000 = 10%)
(define-constant bronze-commission u250)
(define-constant silver-commission u200)
(define-constant gold-commission u150)
(define-constant platinum-commission u100)

(define-map seller-tiers
  { seller: principal }
  {
    current-tier: uint,
    total-volume: uint,
    tier-updated-at: uint,
    monthly-volume: uint,
    month-start: uint,
    consecutive-months: uint,
    tier-benefits-used: uint
  }
)

(define-map tier-benefits
  { seller: principal, benefit-type: (string-ascii 20) }
  {
    benefit-value: uint,
    uses-remaining: uint,
    expires-at: uint
  }
)

(define-read-only (get-seller-tier (seller principal))
  (default-to 
    { 
      current-tier: tier-bronze, 
      total-volume: u0, 
      tier-updated-at: u0,
      monthly-volume: u0,
      month-start: stacks-block-height,
      consecutive-months: u0,
      tier-benefits-used: u0
    }
    (map-get? seller-tiers { seller: seller })
  )
)

(define-read-only (get-tier-commission-rate (tier uint))
  (if (is-eq tier tier-platinum)
    platinum-commission
    (if (is-eq tier tier-gold)
      gold-commission
      (if (is-eq tier tier-silver)
        silver-commission
        bronze-commission
      )
    )
  )
)

(define-read-only (calculate-tier-fee (amount uint) (seller principal))
  (let
    ((seller-tier-data (get-seller-tier seller))
     (tier (get current-tier seller-tier-data))
     (commission-rate (get-tier-commission-rate tier)))
    (/ (* amount commission-rate) u10000)
  )
)

(define-read-only (get-tier-requirements (target-tier uint))
  (if (is-eq target-tier tier-platinum)
    { min-sales: platinum-min-sales, min-transactions: platinum-min-transactions, min-rating: platinum-min-rating }
    (if (is-eq target-tier tier-gold)
      { min-sales: gold-min-sales, min-transactions: gold-min-transactions, min-rating: gold-min-rating }
      (if (is-eq target-tier tier-silver)
        { min-sales: silver-min-sales, min-transactions: silver-min-transactions, min-rating: silver-min-rating }
        { min-sales: bronze-min-sales, min-transactions: u0, min-rating: u0 }
      )
    )
  )
)

(define-read-only (check-tier-eligibility (seller principal) (target-tier uint))
  (let
    ((seller-rep (get-user-reputation seller))
     (seller-tier-data (get-seller-tier seller))
     (requirements (get-tier-requirements target-tier))
     (avg-rating (get-average-rating seller)))
    (and
      (>= (get total-volume seller-tier-data) (get min-sales requirements))
      (>= (get completed-sales seller-rep) (get min-transactions requirements))
      (>= avg-rating (get min-rating requirements))
    )
  )
)

(define-public (update-seller-volume (seller principal) (sale-amount uint))
  (let
    ((current-tier-data (get-seller-tier seller))
     (current-month-start (get month-start current-tier-data))
     (blocks-per-month u4320) ;; Approximately 30 days worth of blocks
     (new-monthly-volume 
       (if (> (- stacks-block-height current-month-start) blocks-per-month)
         sale-amount
         (+ (get monthly-volume current-tier-data) sale-amount)
       ))
     (new-month-start
       (if (> (- stacks-block-height current-month-start) blocks-per-month)
         stacks-block-height
         current-month-start
       )))
    
    (map-set seller-tiers
      { seller: seller }
      (merge current-tier-data {
        total-volume: (+ (get total-volume current-tier-data) sale-amount),
        monthly-volume: new-monthly-volume,
        month-start: new-month-start
      })
    )
    (ok true)
  )
)

(define-public (upgrade-seller-tier (seller principal))
  (let
    ((current-tier-data (get-seller-tier seller))
     (current-tier (get current-tier current-tier-data))
     (next-tier (+ current-tier u1)))
    
    (asserts! (<= next-tier tier-platinum) err-tier-already-max)
    (asserts! (check-tier-eligibility seller next-tier) err-tier-not-eligible)
    
    (map-set seller-tiers
      { seller: seller }
      (merge current-tier-data {
        current-tier: next-tier,
        tier-updated-at: stacks-block-height,
        consecutive-months: (+ (get consecutive-months current-tier-data) u1)
      })
    )
    
    ;; Grant tier benefits
    (unwrap-panic (grant-tier-benefits seller next-tier))
    (ok next-tier)
  )
)

(define-private (grant-tier-benefits (seller principal) (tier uint))
  (begin
    ;; Free listing benefit for higher tiers
    (if (>= tier tier-silver)
      (map-set tier-benefits
        { seller: seller, benefit-type: "free-listings" }
        {
          benefit-value: (if (is-eq tier tier-platinum) u10 u5),
          uses-remaining: (if (is-eq tier tier-platinum) u10 u5),
          expires-at: (+ stacks-block-height u4320)
        }
      )
      true
    )
    
    ;; Priority support for gold and platinum
    (if (>= tier tier-gold)
      (map-set tier-benefits
        { seller: seller, benefit-type: "priority-support" }
        {
          benefit-value: u1,
          uses-remaining: u1,
          expires-at: (+ stacks-block-height u4320)
        }
      )
      true
    )
    
    ;; Featured listing for platinum
    (if (is-eq tier tier-platinum)
      (map-set tier-benefits
        { seller: seller, benefit-type: "featured-listing" }
        {
          benefit-value: u3,
          uses-remaining: u3,
          expires-at: (+ stacks-block-height u4320)
        }
      )
      true
    )
    (ok true)
  )
)

(define-public (use-tier-benefit (benefit-type (string-ascii 20)))
  (let
    ((benefit (unwrap! (map-get? tier-benefits { seller: tx-sender, benefit-type: benefit-type }) (err u124)))
     (uses-left (get uses-remaining benefit)))
    
    (asserts! (> uses-left u0) (err u125))
    (asserts! (> (get expires-at benefit) stacks-block-height) (err u126))
    
    (if (is-eq uses-left u1)
      (map-delete tier-benefits { seller: tx-sender, benefit-type: benefit-type })
      (map-set tier-benefits
        { seller: tx-sender, benefit-type: benefit-type }
        (merge benefit { uses-remaining: (- uses-left u1) })
      )
    )
    (ok true)
  )
)

(define-read-only (get-seller-benefits (seller principal))
  (let
    ((free-listings (map-get? tier-benefits { seller: seller, benefit-type: "free-listings" }))
     (priority-support (map-get? tier-benefits { seller: seller, benefit-type: "priority-support" }))
     (featured-listing (map-get? tier-benefits { seller: seller, benefit-type: "featured-listing" })))
    {
      free-listings: free-listings,
      priority-support: priority-support,
      featured-listing: featured-listing
    }
  )
)

(define-public (downgrade-inactive-sellers)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; This function can be called periodically to downgrade sellers who haven't maintained their tier requirements
    ;; Implementation would check monthly volume and downgrade if necessary
    (ok true)
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
        ;; Use tier-based fee for auction sales
        (let ((tier-fee (calculate-tier-fee final-bid seller))
              (tier-seller-amount (- final-bid tier-fee)))
          (try! (as-contract (stx-transfer? tier-seller-amount (as-contract tx-sender) seller)))
          (try! (as-contract (stx-transfer? tier-fee (as-contract tx-sender) contract-owner)))
          (unwrap-panic (update-seller-volume seller final-bid))
        )
        
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




