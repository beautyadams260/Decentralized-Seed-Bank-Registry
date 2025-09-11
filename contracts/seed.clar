
;; title: seed
;; version:
;; summary:
;; description:

(define-data-var last-seed-id uint u0)
(define-data-var target-seed-for-removal uint u0)

(define-map seeds
  { seed-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    species: (string-ascii 100),
    origin: (string-ascii 50),
    quantity: uint,
    harvest-date: uint,
    verified: bool,
    price: uint,
    for-sale: bool
  }
)

(define-map seed-owners
  { owner: principal }
  { seed-ids: (list 100 uint) }
)

(define-map verifiers
  { address: principal }
  { active: bool }
)

(define-map seed-trades
  { trade-id: uint }
  {
    seller: principal,
    buyer: principal,
    seed-id: uint,
    quantity: uint,
    price: uint,
    status: (string-ascii 20)
  }
)

(define-data-var last-trade-id uint u0)

(define-constant contract-owner tx-sender)

(define-constant err-not-authorized (err u100))
(define-constant err-seed-not-found (err u101))
(define-constant err-insufficient-quantity (err u102))
(define-constant err-not-for-sale (err u103))
(define-constant err-invalid-price (err u104))
(define-constant err-not-owner (err u105))
(define-constant err-already-verified (err u106))
(define-constant err-not-verifier (err u107))
(define-constant err-trade-not-found (err u108))

(define-public (register-seed (name (string-ascii 50)) (species (string-ascii 100)) (origin (string-ascii 50)) (quantity uint) (harvest-date uint) (price uint) (for-sale bool))
  (let
    (
      (new-id (+ (var-get last-seed-id) u1))
      (owner-seeds (default-to { seed-ids: (list) } (map-get? seed-owners { owner: tx-sender })))
    )
    (asserts! (> quantity u0) err-insufficient-quantity)
    (map-set seeds
      { seed-id: new-id }
      {
        owner: tx-sender,
        name: name,
        species: species,
        origin: origin,
        quantity: quantity,
        harvest-date: harvest-date,
        verified: false,
        price: price,
        for-sale: for-sale
      }
    )
    (map-set seed-owners
      { owner: tx-sender }
      { seed-ids: (unwrap! (as-max-len? (append (get seed-ids owner-seeds) new-id) u100) err-not-authorized) }
    )
    (var-set last-seed-id new-id)
    (ok new-id)
  )
)

(define-public (update-seed-details (seed-id uint) (name (string-ascii 50)) (species (string-ascii 100)) (origin (string-ascii 50)) (harvest-date uint))
  (let
    (
      (seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
    )
    (asserts! (is-eq (get owner seed) tx-sender) err-not-owner)
    (map-set seeds
      { seed-id: seed-id }
      (merge seed {
        name: name,
        species: species,
        origin: origin,
        harvest-date: harvest-date
      })
    )
    (ok true)
  )
)

(define-public (update-seed-quantity (seed-id uint) (new-quantity uint))
  (let
    (
      (seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
    )
    (asserts! (is-eq (get owner seed) tx-sender) err-not-owner)
    (asserts! (> new-quantity u0) err-insufficient-quantity)
    (map-set seeds
      { seed-id: seed-id }
      (merge seed { quantity: new-quantity })
    )
    (ok true)
  )
)

(define-public (set-seed-for-sale (seed-id uint) (for-sale bool) (price uint))
  (let
    (
      (seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
    )
    (asserts! (is-eq (get owner seed) tx-sender) err-not-owner)
    (asserts! (or (not for-sale) (> price u0)) err-invalid-price)
    (map-set seeds
      { seed-id: seed-id }
      (merge seed { 
        for-sale: for-sale,
        price: price
      })
    )
    (ok true)
  )
)

(define-public (verify-seed (seed-id uint))
  (let
    (
      (seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
      (is-verifier (default-to { active: false } (map-get? verifiers { address: tx-sender })))
    )
    (asserts! (get active is-verifier) err-not-verifier)
    (asserts! (not (get verified seed)) err-already-verified)
    (map-set seeds
      { seed-id: seed-id }
      (merge seed { verified: true })
    )
    (ok true)
  )
)

(define-public (add-verifier (address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-set verifiers
      { address: address }
      { active: true }
    )
    (ok true)
  )
)

(define-public (remove-verifier (address principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
    (map-set verifiers
      { address: address }
      { active: false }
    )
    (ok true)
  )
)

(define-public (purchase-seed (seed-id uint) (quantity uint))
  (let
    (
      (seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
      (seller (get owner seed))
      (seed-price (get price seed))
      (available-quantity (get quantity seed))
      (new-trade-id (+ (var-get last-trade-id) u1))
    )
    (asserts! (get for-sale seed) err-not-for-sale)
    (asserts! (<= quantity available-quantity) err-insufficient-quantity)
    (asserts! (> quantity u0) err-insufficient-quantity)
    
    (map-set seed-trades
      { trade-id: new-trade-id }
      {
        seller: seller,
        buyer: tx-sender,
        seed-id: seed-id,
        quantity: quantity,
        price: (* quantity seed-price),
        status: "pending"
      }
    )
    
    (var-set last-trade-id new-trade-id)
    (ok new-trade-id)
  )
)

(define-public (confirm-trade (trade-id uint))
  (let
    (
      (trade (unwrap! (map-get? seed-trades { trade-id: trade-id }) err-trade-not-found))
      (seed-id (get seed-id trade))
      (seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
      (quantity (get quantity trade))
      (remaining-quantity (- (get quantity seed) quantity))
    )
    (asserts! (is-eq (get seller trade) tx-sender) err-not-owner)
    (asserts! (is-eq (get status trade) "pending") err-not-authorized)
    
    (if (is-eq remaining-quantity u0)
      (map-delete seeds { seed-id: seed-id })
      (map-set seeds
        { seed-id: seed-id }
        (merge seed { quantity: remaining-quantity })
      )
    )
    
    (map-set seed-trades
      { trade-id: trade-id }
      (merge trade { status: "completed" })
    )
    
    (ok true)
  )
)

(define-read-only (get-seed-details (seed-id uint))
  (map-get? seeds { seed-id: seed-id })
)

(define-read-only (get-seeds-by-owner (owner principal))
  (map-get? seed-owners { owner: owner })
)

(define-read-only (get-trade-details (trade-id uint))
  (map-get? seed-trades { trade-id: trade-id })
)

(define-read-only (is-verifier (address principal))
  (default-to { active: false } (map-get? verifiers { address: address }))
)


(define-public (register-multiple-seeds 
    (names (list 10 (string-ascii 50)))
    (species (list 10 (string-ascii 100)))
    (origins (list 10 (string-ascii 50)))
    (quantities (list 10 uint))
    (harvest-dates (list 10 uint))
    (prices (list 10 uint))
    (for-sales (list 10 bool)))
  (let
    ((seed-count (len names))
     (registered-ids (list)))
    (asserts! (> seed-count u0) err-not-authorized)
    (asserts! (is-eq seed-count (len species)) err-not-authorized)
    (asserts! (is-eq seed-count (len origins)) err-not-authorized)
    (asserts! (is-eq seed-count (len quantities)) err-not-authorized)
    (asserts! (is-eq seed-count (len harvest-dates)) err-not-authorized)
    (asserts! (is-eq seed-count (len prices)) err-not-authorized)
    (asserts! (is-eq seed-count (len for-sales)) err-not-authorized)
    (ok (map register-seed names species origins quantities harvest-dates prices for-sales))
  )
)


(define-map seed-ratings
  { seed-id: uint }
  { 
    total-rating: uint,
    rating-count: uint,
    average-rating: uint
  }
)

(define-public (rate-seed (trade-id uint) (rating uint))
  (let
    ((trade (unwrap! (map-get? seed-trades { trade-id: trade-id }) err-trade-not-found))
     (seed-id (get seed-id trade))
     (current-ratings (default-to { total-rating: u0, rating-count: u0, average-rating: u0 } 
                      (map-get? seed-ratings { seed-id: seed-id }))))
    (asserts! (is-eq (get buyer trade) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status trade) "completed") err-not-authorized)
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-price)
    (map-set seed-ratings
      { seed-id: seed-id }
      {
        total-rating: (+ (get total-rating current-ratings) rating),
        rating-count: (+ (get rating-count current-ratings) u1),
        average-rating: (/ (+ (get total-rating current-ratings) rating) 
                         (+ (get rating-count current-ratings) u1))
      }
    )
    (ok true)
  )
)

(define-read-only (get-seed-rating (seed-id uint))
  (map-get? seed-ratings { seed-id: seed-id })
)

(define-data-var last-batch-id uint u0)

(define-map seed-batches
  { batch-id: uint }
  {
    creator: principal,
    name: (string-ascii 50),
    processing-date: uint,
    storage-location: (string-ascii 100),
    storage-conditions: (string-ascii 200),
    quality-grade: (string-ascii 20),
    seed-count: uint,
    active: bool
  }
)

(define-map batch-seeds
  { batch-id: uint }
  { seed-ids: (list 50 uint) }
)

(define-map seed-to-batch
  { seed-id: uint }
  { batch-id: uint }
)

(define-map batch-quality-tests
  { batch-id: uint }
  {
    germination-rate: uint,
    moisture-content: uint,
    purity-percentage: uint,
    test-date: uint,
    tested-by: principal
  }
)

(define-map user-batches
  { user: principal }
  { batch-ids: (list 20 uint) }
)

(define-constant err-batch-not-found (err u200))
(define-constant err-batch-full (err u201))
(define-constant err-seed-already-batched (err u202))
(define-constant err-batch-inactive (err u203))
(define-constant err-invalid-quality-data (err u204))
(define-constant err-not-batch-creator (err u205))

(define-public (create-batch 
    (name (string-ascii 50))
    (processing-date uint)
    (storage-location (string-ascii 100))
    (storage-conditions (string-ascii 200))
    (quality-grade (string-ascii 20)))
  (let
    ((new-batch-id (+ (var-get last-batch-id) u1))
     (user-batch-list (default-to { batch-ids: (list) } (map-get? user-batches { user: tx-sender }))))
    (map-set seed-batches
      { batch-id: new-batch-id }
      {
        creator: tx-sender,
        name: name,
        processing-date: processing-date,
        storage-location: storage-location,
        storage-conditions: storage-conditions,
        quality-grade: quality-grade,
        seed-count: u0,
        active: true
      }
    )
    (map-set batch-seeds
      { batch-id: new-batch-id }
      { seed-ids: (list) }
    )
    (map-set user-batches
      { user: tx-sender }
      { batch-ids: (unwrap! (as-max-len? (append (get batch-ids user-batch-list) new-batch-id) u20) (err u999)) }
    )
    (var-set last-batch-id new-batch-id)
    (ok new-batch-id)
  )
)

(define-public (add-seed-to-batch (seed-id uint) (batch-id uint))
  (let
    ((batch (unwrap! (map-get? seed-batches { batch-id: batch-id }) err-batch-not-found))
     (batch-seed-list (unwrap! (map-get? batch-seeds { batch-id: batch-id }) err-batch-not-found))
     (current-seeds (get seed-ids batch-seed-list)))
    (asserts! (is-eq (get creator batch) tx-sender) err-not-batch-creator)
    (asserts! (get active batch) err-batch-inactive)
    (asserts! (is-none (map-get? seed-to-batch { seed-id: seed-id })) err-seed-already-batched)
    (asserts! (< (len current-seeds) u50) err-batch-full)
    (map-set batch-seeds
      { batch-id: batch-id }
      { seed-ids: (unwrap! (as-max-len? (append current-seeds seed-id) u50) err-batch-full) }
    )
    (map-set seed-to-batch
      { seed-id: seed-id }
      { batch-id: batch-id }
    )
    (map-set seed-batches
      { batch-id: batch-id }
      (merge batch { seed-count: (+ (get seed-count batch) u1) })
    )
    (ok true)
  )
)

(define-public (remove-seed-from-batch (seed-id uint))
  (let
    ((seed-batch-info (unwrap! (map-get? seed-to-batch { seed-id: seed-id }) err-seed-not-found))
     (batch-id (get batch-id seed-batch-info))
     (batch (unwrap! (map-get? seed-batches { batch-id: batch-id }) err-batch-not-found))
     (batch-seed-list (unwrap! (map-get? batch-seeds { batch-id: batch-id }) err-batch-not-found))
     (current-seeds (get seed-ids batch-seed-list)))
    (asserts! (is-eq (get creator batch) tx-sender) err-not-batch-creator)
    (var-set target-seed-for-removal seed-id)
    (map-set batch-seeds
      { batch-id: batch-id }
      { seed-ids: (filter is-not-target-seed current-seeds) }
    )
    (map-delete seed-to-batch { seed-id: seed-id })
    (map-set seed-batches
      { batch-id: batch-id }
      (merge batch { seed-count: (- (get seed-count batch) u1) })
    )
    (ok true)
  )
)

(define-private (is-not-target-seed (current-seed-id uint))
  (let ((target-seed-id (var-get target-seed-for-removal)))
    (not (is-eq current-seed-id target-seed-id))
  )
)

(define-public (update-batch-storage 
    (batch-id uint)
    (storage-location (string-ascii 100))
    (storage-conditions (string-ascii 200)))
  (let
    ((batch (unwrap! (map-get? seed-batches { batch-id: batch-id }) err-batch-not-found)))
    (asserts! (is-eq (get creator batch) tx-sender) err-not-batch-creator)
    (map-set seed-batches
      { batch-id: batch-id }
      (merge batch {
        storage-location: storage-location,
        storage-conditions: storage-conditions
      })
    )
    (ok true)
  )
)

(define-public (record-quality-test 
    (batch-id uint)
    (germination-rate uint)
    (moisture-content uint)
    (purity-percentage uint))
  (let
    ((batch (unwrap! (map-get? seed-batches { batch-id: batch-id }) err-batch-not-found)))
    (asserts! (is-eq (get creator batch) tx-sender) err-not-batch-creator)
    (asserts! (<= germination-rate u100) err-invalid-quality-data)
    (asserts! (<= moisture-content u100) err-invalid-quality-data)
    (asserts! (<= purity-percentage u100) err-invalid-quality-data)
    (map-set batch-quality-tests
      { batch-id: batch-id }
      {
        germination-rate: germination-rate,
        moisture-content: moisture-content,
        purity-percentage: purity-percentage,
        test-date: stacks-block-height,
        tested-by: tx-sender
      }
    )
    (ok true)
  )
)

(define-public (deactivate-batch (batch-id uint))
  (let
    ((batch (unwrap! (map-get? seed-batches { batch-id: batch-id }) err-batch-not-found)))
    (asserts! (is-eq (get creator batch) tx-sender) err-not-batch-creator)
    (map-set seed-batches
      { batch-id: batch-id }
      (merge batch { active: false })
    )
    (ok true)
  )
)

(define-read-only (get-batch-details (batch-id uint))
  (map-get? seed-batches { batch-id: batch-id })
)

(define-read-only (get-batch-seeds (batch-id uint))
  (map-get? batch-seeds { batch-id: batch-id })
)

(define-read-only (get-seed-batch (seed-id uint))
  (map-get? seed-to-batch { seed-id: seed-id })
)

(define-read-only (get-batch-quality (batch-id uint))
  (map-get? batch-quality-tests { batch-id: batch-id })
)

(define-read-only (get-user-batches (user principal))
  (map-get? user-batches { user: user })
)

(define-data-var last-auction-id uint u0)

(define-map seed-auctions
  { auction-id: uint }
  {
    seller: principal,
    seed-id: uint,
    quantity: uint,
    start-time: uint,
    end-time: uint,
    starting-price: uint,
    current-highest-bid: uint,
    highest-bidder: (optional principal),
    reserve-price: uint,
    status: (string-ascii 20),
    bid-count: uint
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  {
    bid-amount: uint,
    bid-time: uint,
    outbid: bool
  }
)

(define-map user-auctions
  { user: principal }
  { auction-ids: (list 50 uint) }
)

(define-map user-bids
  { user: principal }
  { auction-ids: (list 100 uint) }
)

(define-map auction-bid-history
  { auction-id: uint }
  { bid-history: (list 20 { bidder: principal, amount: uint, time: uint }) }
)

(define-constant err-auction-not-found (err u300))
(define-constant err-auction-ended (err u301))
(define-constant err-auction-not-started (err u302))
(define-constant err-bid-too-low (err u303))
(define-constant err-seller-cannot-bid (err u304))
(define-constant err-auction-not-ended (err u305))
(define-constant err-no-bids (err u306))
(define-constant err-reserve-not-met (err u307))
(define-constant err-auction-already-finalized (err u308))
(define-constant err-invalid-auction-time (err u309))
(define-constant err-seed-not-owned (err u310))

(define-public (create-auction 
    (seed-id uint)
    (quantity uint)
    (duration uint)
    (starting-price uint)
    (reserve-price uint))
  (let
    ((seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
     (new-auction-id (+ (var-get last-auction-id) u1))
     (current-time stacks-block-height)
     (end-time (+ current-time duration))
     (user-auction-list (default-to { auction-ids: (list) } (map-get? user-auctions { user: tx-sender }))))
    (asserts! (is-eq (get owner seed) tx-sender) err-seed-not-owned)
    (asserts! (> quantity u0) err-insufficient-quantity)
    (asserts! (<= quantity (get quantity seed)) err-insufficient-quantity)
    (asserts! (> duration u0) err-invalid-auction-time)
    (asserts! (> starting-price u0) err-invalid-price)
    (asserts! (>= reserve-price starting-price) err-invalid-price)
    (map-set seed-auctions
      { auction-id: new-auction-id }
      {
        seller: tx-sender,
        seed-id: seed-id,
        quantity: quantity,
        start-time: current-time,
        end-time: end-time,
        starting-price: starting-price,
        current-highest-bid: u0,
        highest-bidder: none,
        reserve-price: reserve-price,
        status: "active",
        bid-count: u0
      }
    )
    (map-set auction-bid-history
      { auction-id: new-auction-id }
      { bid-history: (list) }
    )
    (map-set user-auctions
      { user: tx-sender }
      { auction-ids: (unwrap! (as-max-len? (append (get auction-ids user-auction-list) new-auction-id) u50) (err u999)) }
    )
    (var-set last-auction-id new-auction-id)
    (ok new-auction-id)
  )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
  (let
    ((auction (unwrap! (map-get? seed-auctions { auction-id: auction-id }) err-auction-not-found))
     (current-time stacks-block-height)
     (current-highest-bid (get current-highest-bid auction))
     (minimum-bid (if (is-eq current-highest-bid u0) 
                     (get starting-price auction) 
                     (+ current-highest-bid u1)))
     (bid-history (get bid-history (unwrap! (map-get? auction-bid-history { auction-id: auction-id }) err-auction-not-found)))
     (user-bid-list (default-to { auction-ids: (list) } (map-get? user-bids { user: tx-sender }))))
    (asserts! (is-eq (get status auction) "active") err-auction-ended)
    (asserts! (>= current-time (get start-time auction)) err-auction-not-started)
    (asserts! (< current-time (get end-time auction)) err-auction-ended)
    (asserts! (not (is-eq (get seller auction) tx-sender)) err-seller-cannot-bid)
    (asserts! (>= bid-amount minimum-bid) err-bid-too-low)
    (match (get highest-bidder auction)
      previous-bidder (map-set auction-bids
                        { auction-id: auction-id, bidder: previous-bidder }
                        (merge (unwrap! (map-get? auction-bids { auction-id: auction-id, bidder: previous-bidder }) err-auction-not-found) { outbid: true }))
      true
    )
    (map-set auction-bids
      { auction-id: auction-id, bidder: tx-sender }
      {
        bid-amount: bid-amount,
        bid-time: current-time,
        outbid: false
      }
    )
    (map-set seed-auctions
      { auction-id: auction-id }
      (merge auction {
        current-highest-bid: bid-amount,
        highest-bidder: (some tx-sender),
        bid-count: (+ (get bid-count auction) u1)
      })
    )
    (map-set auction-bid-history
      { auction-id: auction-id }
      { bid-history: (unwrap! (as-max-len? (append bid-history { bidder: tx-sender, amount: bid-amount, time: current-time }) u20) (err u999)) }
    )
    (map-set user-bids
      { user: tx-sender }
      { auction-ids: (unwrap! (as-max-len? (append (get auction-ids user-bid-list) auction-id) u100) (err u999)) }
    )
    (ok true)
  )
)

(define-public (finalize-auction (auction-id uint))
  (let
    ((auction (unwrap! (map-get? seed-auctions { auction-id: auction-id }) err-auction-not-found))
     (current-time stacks-block-height)
     (seed-id (get seed-id auction))
     (seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
     (auction-quantity (get quantity auction))
     (remaining-quantity (- (get quantity seed) auction-quantity)))
    (asserts! (>= current-time (get end-time auction)) err-auction-not-ended)
    (asserts! (is-eq (get status auction) "active") err-auction-already-finalized)
    (match (get highest-bidder auction)
      winner (begin
        (asserts! (>= (get current-highest-bid auction) (get reserve-price auction)) err-reserve-not-met)
        (if (is-eq remaining-quantity u0)
          (map-delete seeds { seed-id: seed-id })
          (map-set seeds
            { seed-id: seed-id }
            (merge seed { quantity: remaining-quantity })
          )
        )
        (map-set seed-auctions
          { auction-id: auction-id }
          (merge auction { status: "completed" })
        )
        (ok true)
      )
      (begin
        (map-set seed-auctions
          { auction-id: auction-id }
          (merge auction { status: "expired" })
        )
        (ok false)
      )
    )
  )
)

(define-public (cancel-auction (auction-id uint))
  (let
    ((auction (unwrap! (map-get? seed-auctions { auction-id: auction-id }) err-auction-not-found)))
    (asserts! (is-eq (get seller auction) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status auction) "active") err-auction-already-finalized)
    (asserts! (is-eq (get bid-count auction) u0) err-no-bids)
    (map-set seed-auctions
      { auction-id: auction-id }
      (merge auction { status: "cancelled" })
    )
    (ok true)
  )
)

(define-public (extend-auction (auction-id uint) (additional-time uint))
  (let
    ((auction (unwrap! (map-get? seed-auctions { auction-id: auction-id }) err-auction-not-found))
     (current-time stacks-block-height))
    (asserts! (is-eq (get seller auction) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status auction) "active") err-auction-already-finalized)
    (asserts! (< current-time (get end-time auction)) err-auction-ended)
    (asserts! (> additional-time u0) err-invalid-auction-time)
    (map-set seed-auctions
      { auction-id: auction-id }
      (merge auction { end-time: (+ (get end-time auction) additional-time) })
    )
    (ok true)
  )
)

(define-read-only (get-auction-details (auction-id uint))
  (map-get? seed-auctions { auction-id: auction-id })
)

(define-read-only (get-auction-bid-history (auction-id uint))
  (map-get? auction-bid-history { auction-id: auction-id })
)

(define-read-only (get-user-auction-bids (user principal))
  (map-get? user-bids { user: user })
)

(define-read-only (get-user-auctions (user principal))
  (map-get? user-auctions { user: user })
)

(define-read-only (get-bid-details (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (is-auction-active (auction-id uint))
  (match (map-get? seed-auctions { auction-id: auction-id })
    auction (and (is-eq (get status auction) "active")
                 (>= stacks-block-height (get start-time auction))
                 (< stacks-block-height (get end-time auction)))
    false
  )
)

(define-read-only (get-active-auctions-count)
  (var-get last-auction-id)
)

;; =====================================
;; SEED GENETICS & LINEAGE TRACKING
;; =====================================

(define-data-var last-lineage-id uint u0)

;; Store genetic lineage information for seeds
(define-map seed-lineage
  { seed-id: uint }
  {
    lineage-id: uint,
    parent-seed-1: (optional uint),
    parent-seed-2: (optional uint),
    generation: uint,
    breeding-method: (string-ascii 30),
    breeder: principal,
    breeding-date: uint,
    notes: (string-ascii 200)
  }
)

;; Store genetic traits for seeds
(define-map seed-genetics
  { seed-id: uint }
  {
    drought-resistance: uint,
    cold-tolerance: uint,
    yield-potential: uint,
    disease-resistance: uint,
    maturity-days: uint,
    plant-height: uint,
    genetic-purity: uint,
    documented-traits: (list 10 (string-ascii 50))
  }
)

;; Track breeding programs and their participants
(define-map breeding-programs
  { program-id: uint }
  {
    coordinator: principal,
    program-name: (string-ascii 100),
    target-species: (string-ascii 100),
    objectives: (string-ascii 300),
    start-date: uint,
    active: bool,
    participant-count: uint
  }
)

;; Link seeds to breeding programs
(define-map program-seeds
  { program-id: uint }
  { seed-ids: (list 50 uint) }
)

;; Track program participants
(define-map program-participants
  { program-id: uint }
  { participants: (list 20 principal) }
)

;; Store offspring records for lineage tracking
(define-map seed-offspring
  { parent-seed-id: uint }
  { offspring-ids: (list 30 uint) }
)

;; Breeding program counter
(define-data-var last-program-id uint u0)

;; Error constants for genetics module
(define-constant err-invalid-genetics-data (err u400))
(define-constant err-parent-not-found (err u401))
(define-constant err-lineage-not-found (err u402))
(define-constant err-program-not-found (err u403))
(define-constant err-not-program-coordinator (err u404))
(define-constant err-program-full (err u405))
(define-constant err-already-in-program (err u406))
(define-constant err-program-inactive (err u407))
(define-constant err-invalid-breeding-method (err u408))

;; Record lineage information for a seed
(define-public (record-seed-lineage 
    (seed-id uint)
    (parent-seed-1 (optional uint))
    (parent-seed-2 (optional uint))
    (breeding-method (string-ascii 30))
    (notes (string-ascii 200)))
  (let
    ((seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
     (new-lineage-id (+ (var-get last-lineage-id) u1))
     (generation (calculate-generation parent-seed-1 parent-seed-2)))
    (asserts! (is-eq (get owner seed) tx-sender) err-not-owner)
    (asserts! (validate-breeding-method breeding-method) err-invalid-breeding-method)
    ;; Verify parent seeds exist if provided
    (match parent-seed-1
      parent1 (asserts! (is-some (map-get? seeds { seed-id: parent1 })) err-parent-not-found)
      true
    )
    (match parent-seed-2
      parent2 (asserts! (is-some (map-get? seeds { seed-id: parent2 })) err-parent-not-found)
      true
    )
    (map-set seed-lineage
      { seed-id: seed-id }
      {
        lineage-id: new-lineage-id,
        parent-seed-1: parent-seed-1,
        parent-seed-2: parent-seed-2,
        generation: generation,
        breeding-method: breeding-method,
        breeder: tx-sender,
        breeding-date: stacks-block-height,
        notes: notes
      }
    )
    ;; Update parent offspring records
    (match parent-seed-1
      parent1 (unwrap-panic (update-offspring-record parent1 seed-id))
      true
    )
    (match parent-seed-2
      parent2 (unwrap-panic (update-offspring-record parent2 seed-id))
      true
    )
    (var-set last-lineage-id new-lineage-id)
    (ok new-lineage-id)
  )
)

;; Record genetic traits for a seed
(define-public (record-genetic-traits
    (seed-id uint)
    (drought-resistance uint)
    (cold-tolerance uint)
    (yield-potential uint)
    (disease-resistance uint)
    (maturity-days uint)
    (plant-height uint)
    (genetic-purity uint)
    (documented-traits (list 10 (string-ascii 50))))
  (let
    ((seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found)))
    (asserts! (is-eq (get owner seed) tx-sender) err-not-owner)
    (asserts! (and (<= drought-resistance u100) (<= cold-tolerance u100)) err-invalid-genetics-data)
    (asserts! (and (<= yield-potential u100) (<= disease-resistance u100)) err-invalid-genetics-data)
    (asserts! (and (<= genetic-purity u100) (> maturity-days u0)) err-invalid-genetics-data)
    (asserts! (> plant-height u0) err-invalid-genetics-data)
    (map-set seed-genetics
      { seed-id: seed-id }
      {
        drought-resistance: drought-resistance,
        cold-tolerance: cold-tolerance,
        yield-potential: yield-potential,
        disease-resistance: disease-resistance,
        maturity-days: maturity-days,
        plant-height: plant-height,
        genetic-purity: genetic-purity,
        documented-traits: documented-traits
      }
    )
    (ok true)
  )
)

;; Create a new breeding program
(define-public (create-breeding-program
    (program-name (string-ascii 100))
    (target-species (string-ascii 100))
    (objectives (string-ascii 300)))
  (let
    ((new-program-id (+ (var-get last-program-id) u1)))
    (map-set breeding-programs
      { program-id: new-program-id }
      {
        coordinator: tx-sender,
        program-name: program-name,
        target-species: target-species,
        objectives: objectives,
        start-date: stacks-block-height,
        active: true,
        participant-count: u1
      }
    )
    (map-set program-participants
      { program-id: new-program-id }
      { participants: (list tx-sender) }
    )
    (map-set program-seeds
      { program-id: new-program-id }
      { seed-ids: (list) }
    )
    (var-set last-program-id new-program-id)
    (ok new-program-id)
  )
)

;; Join a breeding program
(define-public (join-breeding-program (program-id uint))
  (let
    ((program (unwrap! (map-get? breeding-programs { program-id: program-id }) err-program-not-found))
     (participants (unwrap! (map-get? program-participants { program-id: program-id }) err-program-not-found))
     (current-participants (get participants participants)))
    (asserts! (get active program) err-program-inactive)
    (asserts! (< (len current-participants) u20) err-program-full)
    (asserts! (is-none (index-of current-participants tx-sender)) err-already-in-program)
    (map-set program-participants
      { program-id: program-id }
      { participants: (unwrap! (as-max-len? (append current-participants tx-sender) u20) err-program-full) }
    )
    (map-set breeding-programs
      { program-id: program-id }
      (merge program { participant-count: (+ (get participant-count program) u1) })
    )
    (ok true)
  )
)

;; Add seed to breeding program
(define-public (add-seed-to-program (seed-id uint) (program-id uint))
  (let
    ((seed (unwrap! (map-get? seeds { seed-id: seed-id }) err-seed-not-found))
     (program (unwrap! (map-get? breeding-programs { program-id: program-id }) err-program-not-found))
     (program-seed-list (unwrap! (map-get? program-seeds { program-id: program-id }) err-program-not-found))
     (participants (unwrap! (map-get? program-participants { program-id: program-id }) err-program-not-found))
     (current-seeds (get seed-ids program-seed-list)))
    (asserts! (is-eq (get owner seed) tx-sender) err-not-owner)
    (asserts! (get active program) err-program-inactive)
    (asserts! (is-some (index-of (get participants participants) tx-sender)) err-not-authorized)
    (asserts! (< (len current-seeds) u50) err-program-full)
    (map-set program-seeds
      { program-id: program-id }
      { seed-ids: (unwrap! (as-max-len? (append current-seeds seed-id) u50) err-program-full) }
    )
    (ok true)
  )
)

;; Update offspring record when new lineage is created
(define-private (update-offspring-record (parent-id uint) (offspring-id uint))
  (let
    ((current-offspring (default-to { offspring-ids: (list) } (map-get? seed-offspring { parent-seed-id: parent-id })))
     (offspring-list (get offspring-ids current-offspring)))
    (map-set seed-offspring
      { parent-seed-id: parent-id }
      { offspring-ids: (unwrap! (as-max-len? (append offspring-list offspring-id) u30) (err u999)) }
    )
    (ok true)
  )
)

;; Calculate generation based on parent generations
(define-private (calculate-generation (parent1 (optional uint)) (parent2 (optional uint)))
  (let
    ((gen1 (match parent1
             p1 (match (map-get? seed-lineage { seed-id: p1 })
                   lineage (get generation lineage)
                   u0)
             u0))
     (gen2 (match parent2
             p2 (match (map-get? seed-lineage { seed-id: p2 })
                   lineage (get generation lineage)
                   u0)
             u0)))
    (+ (if (> gen1 gen2) gen1 gen2) u1)
  )
)

;; Validate breeding method
(define-private (validate-breeding-method (method (string-ascii 30)))
  (or (is-eq method "open-pollination")
      (is-eq method "controlled-cross")
      (is-eq method "self-pollination")
      (is-eq method "hybrid")
      (is-eq method "mutation")
      (is-eq method "selection"))
)

;; Read-only functions for genetics module

(define-read-only (get-seed-lineage (seed-id uint))
  (map-get? seed-lineage { seed-id: seed-id })
)

(define-read-only (get-seed-genetics (seed-id uint))
  (map-get? seed-genetics { seed-id: seed-id })
)

(define-read-only (get-breeding-program (program-id uint))
  (map-get? breeding-programs { program-id: program-id })
)

(define-read-only (get-program-participants (program-id uint))
  (map-get? program-participants { program-id: program-id })
)

(define-read-only (get-program-seeds (program-id uint))
  (map-get? program-seeds { program-id: program-id })
)

(define-read-only (get-seed-offspring (parent-seed-id uint))
  (map-get? seed-offspring { parent-seed-id: parent-seed-id })
)

(define-read-only (get-lineage-count)
  (var-get last-lineage-id)
)

(define-read-only (get-program-count)
  (var-get last-program-id)
)

;; =====================================
;; SEED EXCHANGE NETWORK
;; =====================================

(define-data-var last-exchange-id uint u0)

;; Store seed exchange proposals
(define-map seed-exchanges
  { exchange-id: uint }
  {
    proposer: principal,
    offered-seed-id: uint,
    offered-quantity: uint,
    desired-species: (string-ascii 100),
    desired-origin: (string-ascii 50),
    min-desired-quantity: uint,
    exchange-notes: (string-ascii 200),
    status: (string-ascii 20),
    created-time: uint,
    expires-time: uint,
    accepted-by: (optional principal),
    accepted-seed-id: (optional uint)
  }
)

;; Track exchange responses to proposals
(define-map exchange-responses
  { exchange-id: uint, responder: principal }
  {
    offered-seed-id: uint,
    offered-quantity: uint,
    response-time: uint,
    message: (string-ascii 100)
  }
)

;; Store completed exchange history
(define-map exchange-history
  { exchange-id: uint }
  {
    proposer: principal,
    accepter: principal,
    proposer-seed-id: uint,
    accepter-seed-id: uint,
    proposer-quantity: uint,
    accepter-quantity: uint,
    completion-time: uint
  }
)

;; User exchange preferences and statistics
(define-map user-exchange-profile
  { user: principal }
  {
    preferred-species: (list 5 (string-ascii 100)),
    exchange-region: (string-ascii 50),
    successful-exchanges: uint,
    failed-exchanges: uint,
    reputation-score: uint,
    last-active: uint
  }
)

;; Track user's active exchange proposals
(define-map user-exchanges
  { user: principal }
  { exchange-ids: (list 10 uint) }
)

;; Regional exchange networks for community building
(define-map regional-networks
  { region: (string-ascii 50) }
  {
    active-exchanges: uint,
    total-completed: uint,
    participating-users: uint,
    network-coordinator: (optional principal)
  }
)

;; Error constants for exchange system
(define-constant err-exchange-not-found (err u500))
(define-constant err-exchange-expired (err u501))
(define-constant err-cannot-exchange-own-seed (err u502))
(define-constant err-exchange-already-accepted (err u503))
(define-constant err-insufficient-exchange-quantity (err u504))
(define-constant err-species-mismatch (err u505))
(define-constant err-exchange-not-accepted (err u506))
(define-constant err-not-exchange-participant (err u507))

;; Create a new seed exchange proposal
(define-public (create-exchange-proposal
    (offered-seed-id uint)
    (offered-quantity uint)
    (desired-species (string-ascii 100))
    (desired-origin (string-ascii 50))
    (min-desired-quantity uint)
    (exchange-notes (string-ascii 200))
    (duration uint))
  (let
    ((seed (unwrap! (map-get? seeds { seed-id: offered-seed-id }) err-seed-not-found))
     (new-exchange-id (+ (var-get last-exchange-id) u1))
     (current-time stacks-block-height)
     (user-exchange-list (default-to { exchange-ids: (list) } (map-get? user-exchanges { user: tx-sender }))))
    (asserts! (is-eq (get owner seed) tx-sender) err-not-owner)
    (asserts! (<= offered-quantity (get quantity seed)) err-insufficient-quantity)
    (asserts! (> offered-quantity u0) err-insufficient-quantity)
    (asserts! (> min-desired-quantity u0) err-insufficient-exchange-quantity)
    (asserts! (> duration u0) err-invalid-auction-time)
    (map-set seed-exchanges
      { exchange-id: new-exchange-id }
      {
        proposer: tx-sender,
        offered-seed-id: offered-seed-id,
        offered-quantity: offered-quantity,
        desired-species: desired-species,
        desired-origin: desired-origin,
        min-desired-quantity: min-desired-quantity,
        exchange-notes: exchange-notes,
        status: "active",
        created-time: current-time,
        expires-time: (+ current-time duration),
        accepted-by: none,
        accepted-seed-id: none
      }
    )
    (map-set user-exchanges
      { user: tx-sender }
      { exchange-ids: (unwrap! (as-max-len? (append (get exchange-ids user-exchange-list) new-exchange-id) u10) (err u999)) }
    )
    (var-set last-exchange-id new-exchange-id)
    (ok new-exchange-id)
  )
)

;; Respond to an exchange proposal with a counter-offer
(define-public (respond-to-exchange
    (exchange-id uint)
    (offered-seed-id uint)
    (offered-quantity uint)
    (message (string-ascii 100)))
  (let
    ((exchange (unwrap! (map-get? seed-exchanges { exchange-id: exchange-id }) err-exchange-not-found))
     (offered-seed (unwrap! (map-get? seeds { seed-id: offered-seed-id }) err-seed-not-found))
     (current-time stacks-block-height))
    (asserts! (is-eq (get status exchange) "active") err-exchange-expired)
    (asserts! (< current-time (get expires-time exchange)) err-exchange-expired)
    (asserts! (not (is-eq (get proposer exchange) tx-sender)) err-cannot-exchange-own-seed)
    (asserts! (is-eq (get owner offered-seed) tx-sender) err-not-owner)
    (asserts! (<= offered-quantity (get quantity offered-seed)) err-insufficient-quantity)
    (asserts! (>= offered-quantity (get min-desired-quantity exchange)) err-insufficient-exchange-quantity)
    (asserts! (is-eq (get species offered-seed) (get desired-species exchange)) err-species-mismatch)
    (map-set exchange-responses
      { exchange-id: exchange-id, responder: tx-sender }
      {
        offered-seed-id: offered-seed-id,
        offered-quantity: offered-quantity,
        response-time: current-time,
        message: message
      }
    )
    (ok true)
  )
)

;; Accept a specific exchange response
(define-public (accept-exchange-response
    (exchange-id uint)
    (responder principal))
  (let
    ((exchange (unwrap! (map-get? seed-exchanges { exchange-id: exchange-id }) err-exchange-not-found))
     (response (unwrap! (map-get? exchange-responses { exchange-id: exchange-id, responder: responder }) err-exchange-not-found)))
    (asserts! (is-eq (get proposer exchange) tx-sender) err-not-authorized)
    (asserts! (is-eq (get status exchange) "active") err-exchange-already-accepted)
    (map-set seed-exchanges
      { exchange-id: exchange-id }
      (merge exchange {
        status: "accepted",
        accepted-by: (some responder),
        accepted-seed-id: (some (get offered-seed-id response))
      })
    )
    (ok true)
  )
)

;; Complete the seed exchange
(define-public (complete-exchange (exchange-id uint))
  (let
    ((exchange (unwrap! (map-get? seed-exchanges { exchange-id: exchange-id }) err-exchange-not-found))
     (accepter (unwrap! (get accepted-by exchange) err-exchange-not-accepted))
     (accepted-seed-id (unwrap! (get accepted-seed-id exchange) err-exchange-not-accepted))
     (response (unwrap! (map-get? exchange-responses { exchange-id: exchange-id, responder: accepter }) err-exchange-not-found))
     (proposer-seed (unwrap! (map-get? seeds { seed-id: (get offered-seed-id exchange) }) err-seed-not-found))
     (accepter-seed (unwrap! (map-get? seeds { seed-id: accepted-seed-id }) err-seed-not-found))
     (proposer-remaining (- (get quantity proposer-seed) (get offered-quantity exchange)))
     (accepter-remaining (- (get quantity accepter-seed) (get offered-quantity response))))
    (asserts! (or (is-eq tx-sender (get proposer exchange)) (is-eq tx-sender accepter)) err-not-exchange-participant)
    (asserts! (is-eq (get status exchange) "accepted") err-exchange-not-accepted)
    ;; Update seed quantities or remove if depleted
    (if (is-eq proposer-remaining u0)
      (map-delete seeds { seed-id: (get offered-seed-id exchange) })
      (map-set seeds { seed-id: (get offered-seed-id exchange) }
        (merge proposer-seed { quantity: proposer-remaining }))
    )
    (if (is-eq accepter-remaining u0)
      (map-delete seeds { seed-id: accepted-seed-id })
      (map-set seeds { seed-id: accepted-seed-id }
        (merge accepter-seed { quantity: accepter-remaining }))
    )
    ;; Record exchange completion
    (map-set exchange-history
      { exchange-id: exchange-id }
      {
        proposer: (get proposer exchange),
        accepter: accepter,
        proposer-seed-id: (get offered-seed-id exchange),
        accepter-seed-id: accepted-seed-id,
        proposer-quantity: (get offered-quantity exchange),
        accepter-quantity: (get offered-quantity response),
        completion-time: stacks-block-height
      }
    )
    (map-set seed-exchanges
      { exchange-id: exchange-id }
      (merge exchange { status: "completed" })
    )
    (ok true)
  )
)

;; Update user exchange preferences
(define-public (update-exchange-profile
    (preferred-species (list 5 (string-ascii 100)))
    (exchange-region (string-ascii 50)))
  (let
    ((current-profile (default-to
       {
         preferred-species: (list),
         exchange-region: "",
         successful-exchanges: u0,
         failed-exchanges: u0,
         reputation-score: u50,
         last-active: u0
       }
       (map-get? user-exchange-profile { user: tx-sender }))))
    (map-set user-exchange-profile
      { user: tx-sender }
      (merge current-profile {
        preferred-species: preferred-species,
        exchange-region: exchange-region,
        last-active: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Read-only functions for exchange system
(define-read-only (get-exchange-details (exchange-id uint))
  (map-get? seed-exchanges { exchange-id: exchange-id })
)

(define-read-only (get-exchange-response (exchange-id uint) (responder principal))
  (map-get? exchange-responses { exchange-id: exchange-id, responder: responder })
)

(define-read-only (get-exchange-history (exchange-id uint))
  (map-get? exchange-history { exchange-id: exchange-id })
)

(define-read-only (get-user-exchange-profile (user principal))
  (map-get? user-exchange-profile { user: user })
)

(define-read-only (get-user-exchanges (user principal))
  (map-get? user-exchanges { user: user })
)

(define-read-only (get-regional-network (region (string-ascii 50)))
  (map-get? regional-networks { region: region })
)

(define-read-only (get-total-exchanges)
  (var-get last-exchange-id)
)

