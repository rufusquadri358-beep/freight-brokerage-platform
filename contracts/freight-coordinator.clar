;; Freight Brokerage Platform Smart Contract
;; Coordinates shipping loads, carriers, rate negotiation, and delivery tracking

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-already-exists (err u105))

;; Data structures
(define-data-var next-load-id uint u1)
(define-data-var next-carrier-id uint u1)

;; Load status types
(define-constant status-posted u1)
(define-constant status-matched u2)
(define-constant status-in-transit u3)
(define-constant status-delivered u4)
(define-constant status-cancelled u5)

;; Load information
(define-map loads
  { load-id: uint }
  {
    shipper: principal,
    origin: (string-ascii 100),
    destination: (string-ascii 100),
    cargo-description: (string-ascii 200),
    weight-lbs: uint,
    offered-rate: uint,
    pickup-date: uint,
    delivery-date: uint,
    status: uint,
    assigned-carrier: (optional principal),
    negotiated-rate: (optional uint),
    created-at: uint
  }
)

;; Carrier registration
(define-map carriers
  { carrier-id: uint }
  {
    carrier-address: principal,
    company-name: (string-ascii 100),
    mc-number: (string-ascii 20),
    insurance-amount: uint,
    rating: uint,
    total-loads: uint,
    active: bool,
    registered-at: uint
  }
)

;; Carrier principal to ID mapping
(define-map carrier-principals
  { carrier: principal }
  { carrier-id: uint }
)

;; Bids from carriers on loads
(define-map load-bids
  { load-id: uint, carrier: principal }
  {
    bid-rate: uint,
    message: (string-ascii 200),
    bid-time: uint
  }
)

;; Delivery tracking
(define-map delivery-updates
  { load-id: uint, update-id: uint }
  {
    carrier: principal,
    location: (string-ascii 100),
    status-message: (string-ascii 200),
    timestamp: uint,
    proof-hash: (optional (buff 32))
  }
)

(define-map load-update-count
  { load-id: uint }
  { count: uint }
)

;; Payment escrow
(define-map load-payments
  { load-id: uint }
  {
    amount: uint,
    released: bool,
    release-time: (optional uint)
  }
)

;; Read-only functions
(define-read-only (get-load (load-id uint))
  (map-get? loads { load-id: load-id })
)

(define-read-only (get-carrier (carrier-id uint))
  (map-get? carriers { carrier-id: carrier-id })
)

(define-read-only (get-carrier-by-principal (carrier principal))
  (match (map-get? carrier-principals { carrier: carrier })
    carrier-data (map-get? carriers { carrier-id: (get carrier-id carrier-data) })
    none
  )
)

(define-read-only (get-load-bid (load-id uint) (carrier principal))
  (map-get? load-bids { load-id: load-id, carrier: carrier })
)

(define-read-only (get-delivery-update (load-id uint) (update-id uint))
  (map-get? delivery-updates { load-id: load-id, update-id: update-id })
)

(define-read-only (get-load-payment (load-id uint))
  (map-get? load-payments { load-id: load-id })
)

(define-read-only (get-next-load-id)
  (var-get next-load-id)
)

(define-read-only (get-next-carrier-id)
  (var-get next-carrier-id)
)

;; Public functions

;; Register as a carrier
(define-public (register-carrier (company-name (string-ascii 100)) (mc-number (string-ascii 20)) (insurance-amount uint))
  (let
    (
      (carrier-id (var-get next-carrier-id))
      (current-block-height stacks-block-height)
    )
    (asserts! (is-none (map-get? carrier-principals { carrier: tx-sender })) err-already-exists)
    (map-set carriers
      { carrier-id: carrier-id }
      {
        carrier-address: tx-sender,
        company-name: company-name,
        mc-number: mc-number,
        insurance-amount: insurance-amount,
        rating: u50, ;; Default rating out of 100
        total-loads: u0,
        active: true,
        registered-at: current-block-height
      }
    )
    (map-set carrier-principals
      { carrier: tx-sender }
      { carrier-id: carrier-id }
    )
    (var-set next-carrier-id (+ carrier-id u1))
    (ok carrier-id)
  )
)

;; Post a new load
(define-public (post-load 
  (origin (string-ascii 100))
  (destination (string-ascii 100))
  (cargo-description (string-ascii 200))
  (weight-lbs uint)
  (offered-rate uint)
  (pickup-date uint)
  (delivery-date uint)
)
  (let
    (
      (load-id (var-get next-load-id))
      (current-block-height stacks-block-height)
    )
    (map-set loads
      { load-id: load-id }
      {
        shipper: tx-sender,
        origin: origin,
        destination: destination,
        cargo-description: cargo-description,
        weight-lbs: weight-lbs,
        offered-rate: offered-rate,
        pickup-date: pickup-date,
        delivery-date: delivery-date,
        status: status-posted,
        assigned-carrier: none,
        negotiated-rate: none,
        created-at: current-block-height
      }
    )
    (var-set next-load-id (+ load-id u1))
    (ok load-id)
  )
)

;; Carriers bid on loads
(define-public (submit-bid (load-id uint) (bid-rate uint) (message (string-ascii 200)))
  (let
    (
      (load-data (unwrap! (map-get? loads { load-id: load-id }) err-not-found))
      (carrier-data (unwrap! (get-carrier-by-principal tx-sender) err-unauthorized))
    )
    (asserts! (is-eq (get status load-data) status-posted) err-invalid-status)
    (asserts! (get active carrier-data) err-unauthorized)
    (map-set load-bids
      { load-id: load-id, carrier: tx-sender }
      {
        bid-rate: bid-rate,
        message: message,
        bid-time: stacks-block-height
      }
    )
    (ok true)
  )
)

;; Shipper accepts a bid and assigns carrier
(define-public (accept-bid (load-id uint) (carrier principal) (final-rate uint))
  (let
    (
      (load-data (unwrap! (map-get? loads { load-id: load-id }) err-not-found))
      (bid-data (unwrap! (map-get? load-bids { load-id: load-id, carrier: carrier }) err-not-found))
    )
    (asserts! (is-eq (get shipper load-data) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status load-data) status-posted) err-invalid-status)
    (map-set loads
      { load-id: load-id }
      (merge load-data {
        status: status-matched,
        assigned-carrier: (some carrier),
        negotiated-rate: (some final-rate)
      })
    )
    (ok true)
  )
)

;; Carrier starts transport
(define-public (start-transport (load-id uint))
  (let
    (
      (load-data (unwrap! (map-get? loads { load-id: load-id }) err-not-found))
    )
    (asserts! (is-eq (some tx-sender) (get assigned-carrier load-data)) err-unauthorized)
    (asserts! (is-eq (get status load-data) status-matched) err-invalid-status)
    (map-set loads
      { load-id: load-id }
      (merge load-data { status: status-in-transit })
    )
    (ok true)
  )
)

;; Add delivery tracking update
(define-public (add-delivery-update 
  (load-id uint)
  (location (string-ascii 100))
  (status-message (string-ascii 200))
  (proof-hash (optional (buff 32)))
)
  (let
    (
      (load-data (unwrap! (map-get? loads { load-id: load-id }) err-not-found))
      (current-count (default-to u0 (get count (map-get? load-update-count { load-id: load-id }))))
      (next-update-id (+ current-count u1))
    )
    (asserts! (is-eq (some tx-sender) (get assigned-carrier load-data)) err-unauthorized)
    (asserts! (or (is-eq (get status load-data) status-in-transit) 
                  (is-eq (get status load-data) status-matched)) err-invalid-status)
    (map-set delivery-updates
      { load-id: load-id, update-id: next-update-id }
      {
        carrier: tx-sender,
        location: location,
        status-message: status-message,
        timestamp: stacks-block-height,
        proof-hash: proof-hash
      }
    )
    (map-set load-update-count
      { load-id: load-id }
      { count: next-update-id }
    )
    (ok next-update-id)
  )
)

;; Mark delivery as complete
(define-public (complete-delivery (load-id uint))
  (let
    (
      (load-data (unwrap! (map-get? loads { load-id: load-id }) err-not-found))
      (carrier-principal (unwrap! (get assigned-carrier load-data) err-not-found))
      (carrier-lookup (unwrap! (map-get? carrier-principals { carrier: carrier-principal }) err-not-found))
      (carrier-data (unwrap! (map-get? carriers { carrier-id: (get carrier-id carrier-lookup) }) err-not-found))
    )
    (asserts! (is-eq (some tx-sender) (get assigned-carrier load-data)) err-unauthorized)
    (asserts! (is-eq (get status load-data) status-in-transit) err-invalid-status)
    (map-set loads
      { load-id: load-id }
      (merge load-data { status: status-delivered })
    )
    ;; Update carrier stats
    (map-set carriers
      { carrier-id: (get carrier-id carrier-lookup) }
      (merge carrier-data { total-loads: (+ (get total-loads carrier-data) u1) })
    )
    (ok true)
  )
)

;; Shipper can cancel posted loads
(define-public (cancel-load (load-id uint))
  (let
    (
      (load-data (unwrap! (map-get? loads { load-id: load-id }) err-not-found))
    )
    (asserts! (is-eq (get shipper load-data) tx-sender) err-unauthorized)
    (asserts! (is-eq (get status load-data) status-posted) err-invalid-status)
    (map-set loads
      { load-id: load-id }
      (merge load-data { status: status-cancelled })
    )
    (ok true)
  )
)

;; Update carrier rating (simplified - in production would have more complex logic)
(define-public (update-carrier-rating (carrier-id uint) (new-rating uint))
  (let
    (
      (carrier-data (unwrap! (map-get? carriers { carrier-id: carrier-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rating u100) err-invalid-status)
    (map-set carriers
      { carrier-id: carrier-id }
      (merge carrier-data { rating: new-rating })
    )
    (ok true)
  )
)


