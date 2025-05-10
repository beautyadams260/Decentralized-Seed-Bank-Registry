
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


