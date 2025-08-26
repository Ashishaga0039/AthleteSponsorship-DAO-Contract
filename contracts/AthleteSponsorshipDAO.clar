;; AthleteSponsorship DAO Contract
;; Direct fan funding for athletes with performance-based returns and exclusive access
;; A decentralized platform connecting fans directly with athletes

;; Define the sponsorship token for rewards
(define-fungible-token sponsorship-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-unauthorized (err u101))
(define-constant err-insufficient-funds (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-athlete-not-found (err u104))
(define-constant err-sponsorship-not-found (err u105))

;; Data variables
(define-data-var next-athlete-id uint u1)
(define-data-var next-sponsorship-id uint u1)
(define-data-var total-platform-fees uint u0)

;; Data maps
(define-map athletes
  { athlete-id: uint }
  {
    wallet: principal,
    name: (string-ascii 50),
    sport: (string-ascii 30),
    total-raised: uint,
    active: bool,
    performance-multiplier: uint ;; Multiplier for performance-based returns (100 = 1.0x)
  }
)

(define-map sponsorships
  { sponsorship-id: uint }
  {
    sponsor: principal,
    athlete-id: uint,
    amount: uint,
    timestamp: uint,
    tokens-earned: uint,
    claimed: bool
  }
)

(define-map athlete-sponsors
  { athlete-id: uint, sponsor: principal }
  { total-sponsored: uint, exclusive-access: bool }
)

;; Function 1: Sponsor Athlete
;; Allows fans to sponsor athletes with STX and receive sponsorship tokens
(define-public (sponsor-athlete (athlete-id uint) (amount uint))
  (let (
    (athlete-data (unwrap! (map-get? athletes { athlete-id: athlete-id }) err-athlete-not-found))
    (sponsorship-id (var-get next-sponsorship-id))
    (platform-fee (/ amount u100)) ;; 1% platform fee
    (net-amount (- amount platform-fee))
    (tokens-to-mint (* amount (get performance-multiplier athlete-data))) ;; Performance-based token calculation
    (current-sponsor-data (default-to 
      { total-sponsored: u0, exclusive-access: false }
      (map-get? athlete-sponsors { athlete-id: athlete-id, sponsor: tx-sender })
    ))
  )
    (begin
      ;; Validate inputs
      (asserts! (> amount u0) err-invalid-amount)
      (asserts! (get active athlete-data) err-athlete-not-found)
      
      ;; Transfer STX to athlete (minus platform fee)
      (try! (stx-transfer? net-amount tx-sender (get wallet athlete-data)))
      
      ;; Transfer platform fee to contract
      (try! (stx-transfer? platform-fee tx-sender (as-contract tx-sender)))
      
      ;; Mint sponsorship tokens to sponsor
      (try! (ft-mint? sponsorship-token tokens-to-mint tx-sender))
      
      ;; Record sponsorship
      (map-set sponsorships 
        { sponsorship-id: sponsorship-id }
        {
          sponsor: tx-sender,
          athlete-id: athlete-id,
          amount: amount,
          timestamp: stacks-block-height,
          tokens-earned: tokens-to-mint,
          claimed: false
        }
      )
      
      ;; Update athlete total raised
      (map-set athletes
        { athlete-id: athlete-id }
        (merge athlete-data { total-raised: (+ (get total-raised athlete-data) amount) })
      )
      
      ;; Update sponsor data (grant exclusive access if sponsored >= 1000 STX total)
      (let ((new-total (+ (get total-sponsored current-sponsor-data) amount)))
        (map-set athlete-sponsors
          { athlete-id: athlete-id, sponsor: tx-sender }
          {
            total-sponsored: new-total,
            exclusive-access: (>= new-total u1000000) ;; 1000 STX in microSTX
          }
        )
      )
      
      ;; Update platform tracking
      (var-set next-sponsorship-id (+ sponsorship-id u1))
      (var-set total-platform-fees (+ (var-get total-platform-fees) platform-fee))
      
      ;; Print sponsorship event
      (print {
        event: "athlete-sponsored",
        sponsor: tx-sender,
        athlete-id: athlete-id,
        amount: amount,
        tokens-earned: tokens-to-mint,
        sponsorship-id: sponsorship-id
      })
      
      (ok {
        sponsorship-id: sponsorship-id,
        tokens-earned: tokens-to-mint,
        exclusive-access-granted: (>= (+ (get total-sponsored current-sponsor-data) amount) u1000000)
      })
    )
  )
)

;; Function 2: Register Athlete
;; Allows new athletes to register on the platform
(define-public (register-athlete (name (string-ascii 50)) (sport (string-ascii 30)) (performance-multiplier uint))
  (let (
    (athlete-id (var-get next-athlete-id))
  )
    (begin
      ;; Validate inputs
      (asserts! (> (len name) u0) err-invalid-amount)
      (asserts! (> (len sport) u0) err-invalid-amount)
      (asserts! (and (>= performance-multiplier u50) (<= performance-multiplier u200)) err-invalid-amount) ;; 0.5x to 2.0x multiplier
      
      ;; Register athlete
      (map-set athletes
        { athlete-id: athlete-id }
        {
          wallet: tx-sender,
          name: name,
          sport: sport,
          total-raised: u0,
          active: true,
          performance-multiplier: performance-multiplier
        }
      )
      
      ;; Update athlete counter
      (var-set next-athlete-id (+ athlete-id u1))
      
      ;; Print registration event
      (print {
        event: "athlete-registered",
        athlete-id: athlete-id,
        wallet: tx-sender,
        name: name,
        sport: sport
      })
      
      (ok athlete-id)
    )
  )
)

;; Read-only functions for data access
(define-read-only (get-athlete (athlete-id uint))
  (map-get? athletes { athlete-id: athlete-id })
)

(define-read-only (get-sponsorship (sponsorship-id uint))
  (map-get? sponsorships { sponsorship-id: sponsorship-id })
)

(define-read-only (get-sponsor-info (athlete-id uint) (sponsor principal))
  (map-get? athlete-sponsors { athlete-id: athlete-id, sponsor: sponsor })
)

(define-read-only (get-sponsorship-token-balance (account principal))
  (ft-get-balance sponsorship-token account)
)

(define-read-only (get-total-athletes)
  (- (var-get next-athlete-id) u1)
)

(define-read-only (get-total-sponsorships)
  (- (var-get next-sponsorship-id) u1)
)

(define-read-only (get-platform-stats)
  (ok {
    total-athletes: (- (var-get next-athlete-id) u1),
    total-sponsorships: (- (var-get next-sponsorship-id) u1),
    platform-fees-collected: (var-get total-platform-fees)
  })
)
