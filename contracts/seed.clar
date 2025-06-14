
;; title: seed
;; version:
;; summary:
;; description:

(define-data-var last-seed-id uint u0)

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

(define-public (rate-seed (seed-id uint) (rating uint))
  (let
    ((trade (unwrap! (map-get? seed-trades { trade-id: seed-id }) err-trade-not-found))
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

(define-private (is-not-target-seed (seed-id uint))
  (not (is-eq seed-id seed-id))
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

