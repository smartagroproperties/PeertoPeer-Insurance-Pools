(define-data-var pool-id uint u0)
(define-data-var admin principal tx-sender)
(define-data-var min-contribution uint u1000000)
(define-data-var claim-threshold uint u500000)
(define-data-var voting-period uint u144)
(define-data-var profit-sharing-percentage uint u5)

(define-map pools
  { id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    creator: principal,
    active: bool,
    total-funds: uint,
    member-count: uint,
    creation-block: uint,
    coverage-amount: uint
  }
)

(define-map pool-members
  { pool-id: uint, member: principal }
  {
    contribution: uint,
    joined-block: uint,
    active: bool,
    claims-filed: uint,
    claims-approved: uint
  }
)

(define-map claims
  { pool-id: uint, claim-id: uint }
  {
    claimant: principal,
    amount: uint,
    description: (string-ascii 200),
    filed-block: uint,
    status: (string-ascii 20),
    votes-yes: uint,
    votes-no: uint,
    paid: bool
  }
)

(define-map claim-votes
  { pool-id: uint, claim-id: uint, voter: principal }
  { vote: bool }
)

(define-map pool-claim-counter
  { pool-id: uint }
  { counter: uint }
)

(define-read-only (get-pool (id uint))
  (map-get? pools { id: id })
)

(define-read-only (get-pool-member (pool-ids uint) (member principal))
  (map-get? pool-members { pool-id: pool-ids, member: member })
)

(define-read-only (get-claim (pool-idss uint) (claim-id uint))
  (map-get? claims { pool-id: pool-idss, claim-id: claim-id })
)

(define-read-only (get-member-vote (pool-idsss uint) (claim-id uint) (voter principal))
  (map-get? claim-votes { pool-id: pool-idsss, claim-id: claim-id, voter: voter })
)

(define-read-only (get-admin)
  (var-get admin)
)

(define-read-only (get-min-contribution)
  (var-get min-contribution)
)

(define-read-only (get-claim-threshold)
  (var-get claim-threshold)
)

(define-public (create-pool (name (string-ascii 50)) (description (string-ascii 200)) (coverage-amount uint))
  (let
    ((new-pool-id (var-get pool-id)))
    (asserts! (not (var-get pool-paused)) (err u20))

    (asserts! (>= coverage-amount (var-get min-contribution)) (err u1))
    (map-set pools
      { id: new-pool-id }
      {
        name: name,
        description: description,
        creator: tx-sender,
        active: true,
        total-funds: u0,
        member-count: u0,
        creation-block: stacks-block-height,
        coverage-amount: coverage-amount
      }
    )
    (map-set pool-claim-counter
      { pool-id: new-pool-id }
      { counter: u0 }
    )
    (var-set pool-id (+ new-pool-id u1))
    (ok new-pool-id)
  )
)

(define-public (join-pool (pool-idd uint) (amount uint))
  (let
    ((pool (unwrap! (get-pool pool-idd) (err u2)))
     (member-data (get-pool-member pool-idd tx-sender)))
    (asserts! (not (var-get pool-paused)) (err u20))

    (asserts! (get active pool) (err u3))
    (asserts! (>= amount (var-get min-contribution)) (err u4))
    
    (if (is-some member-data)
      (let ((existing-member (unwrap-panic member-data)))
        (asserts! (not (get active existing-member)) (err u5))
        (map-set pool-members
          { pool-id: pool-idd, member: tx-sender }
          {
            contribution: amount,
            joined-block: stacks-block-height,
            active: true,
            claims-filed: (get claims-filed existing-member),
            claims-approved: (get claims-approved existing-member)
          }
        )
      )
      (map-set pool-members
        { pool-id: pool-idd, member: tx-sender }
        {
          contribution: amount,
          joined-block: stacks-block-height,
          active: true,
          claims-filed: u0,
          claims-approved: u0
        }
      )
    )
    
    (map-set pools
      { id: pool-idd }
      (merge pool {
        total-funds: (+ (get total-funds pool) amount),
        member-count: (+ (get member-count pool) u1)
      })
    )
    
    (stx-transfer? amount tx-sender (as-contract tx-sender))
  )
)

(define-public (leave-pool (pool-idd uint))
  (let
    ((pool (unwrap! (get-pool pool-idd) (err u2)))
     (member-data (unwrap! (get-pool-member pool-idd tx-sender) (err u6))))
    
    (asserts! (get active member-data) (err u7))
    
    (let
      ((refund-amount (get contribution member-data)))
      
      (map-set pool-members
        { pool-id: pool-idd, member: tx-sender }
        (merge member-data { active: false })
      )
      
      (map-set pools
        { id: pool-idd }
        (merge pool {
          total-funds: (- (get total-funds pool) refund-amount),
          member-count: (- (get member-count pool) u1)
        })
      )
      
      (as-contract (stx-transfer? refund-amount tx-sender tx-sender))
    )
  )
)

(define-public (file-claim (pool-i uint) (amount uint) (description (string-ascii 200)))
  (let
    ((pool (unwrap! (get-pool pool-i) (err u2)))
     (member-data (unwrap! (get-pool-member pool-i tx-sender) (err u6)))
     (claim-counter (unwrap! (map-get? pool-claim-counter { pool-id: pool-i }) (err u8))))
    (asserts! (not (var-get pool-paused)) (err u20))

    (asserts! (get active pool) (err u3))
    (asserts! (get active member-data) (err u7))
    (asserts! (<= amount (get coverage-amount pool)) (err u9))
    
    (let
      ((new-claim-id (get counter claim-counter)))
      
      (map-set claims
        { pool-id: pool-i, claim-id: new-claim-id }
        {
          claimant: tx-sender,
          amount: amount,
          description: description,
          filed-block: stacks-block-height,
          status: "pending",
          votes-yes: u0,
          votes-no: u0,
          paid: false
        }
      )
      
      (map-set pool-claim-counter
        { pool-id: pool-i }
        { counter: (+ new-claim-id u1) }
      )
      
      (map-set pool-members
        { pool-id: pool-i, member: tx-sender }
        (merge member-data {
          claims-filed: (+ (get claims-filed member-data) u1)
        })
      )
      
      (ok new-claim-id)
    )
  )
)

(define-public (vote-on-claim (pool-t uint) (claim-id uint) (vote bool))
  (let
    ((pool (unwrap! (get-pool pool-t) (err u2)))
     (member-data (unwrap! (get-pool-member pool-t tx-sender) (err u6)))
     (claim-data (unwrap! (get-claim pool-t claim-id) (err u10))))
    
    (asserts! (get active pool) (err u3))
    (asserts! (get active member-data) (err u7))
    (asserts! (is-eq (get status claim-data) "pending") (err u11))
    (asserts! (not (is-eq (get claimant claim-data) tx-sender)) (err u12))
    (asserts! (is-none (get-member-vote pool-t claim-id tx-sender)) (err u13))
    
    (map-set claim-votes
      { pool-id: pool-t, claim-id: claim-id, voter: tx-sender }
      { vote: vote }
    )
    
    (map-set claims
      { pool-id: pool-t, claim-id: claim-id }
      (merge claim-data {
        votes-yes: (+ (get votes-yes claim-data) (if vote u1 u0)),
        votes-no: (+ (get votes-no claim-data) (if vote u0 u1))
      })
    )
    
    (ok true)
  )
)

(define-public (process-claim (pool- uint) (claim-id uint))
  (let
    ((pool (unwrap! (get-pool pool-) (err u2)))
     (claim-data (unwrap! (get-claim pool- claim-id) (err u10))))
    (asserts! (not (var-get pool-paused)) (err u20))

    (asserts! (get active pool) (err u3))
    (asserts! (is-eq (get status claim-data) "pending") (err u11))
    (asserts! (>= (- stacks-block-height (get filed-block claim-data)) (var-get voting-period)) (err u14))
    
    (if (> (get votes-yes claim-data) (get votes-no claim-data))
      (begin
        (map-set claims
          { pool-id: pool-, claim-id: claim-id }
          (merge claim-data {
            status: "approved",
            paid: true
          })
        )

              (let ((stats (unwrap! (get-pool-statistics pool-) (err u22))))
        (map-set pool-statistics
          { pool-id: pool- }
          (merge stats {
            total-claims-approved: (+ (get total-claims-approved stats) u1),
            total-amount-paid: (+ (get total-amount-paid stats) (get amount claim-data)),
            last-activity-block: stacks-block-height
          })
        )
      )

        
        (let
          ((claimant-data (unwrap! (get-pool-member pool- (get claimant claim-data)) (err u15))))
          
          (map-set pool-members
            { pool-id: pool-, member: (get claimant claim-data) }
            (merge claimant-data {
              claims-approved: (+ (get claims-approved claimant-data) u1)
            })
          )
          
          (map-set pools
            { id: pool- }
            (merge pool {
              total-funds: (- (get total-funds pool) (get amount claim-data))
            })
          )
          
          (unwrap! (as-contract (stx-transfer? (get amount claim-data) tx-sender (get claimant claim-data))) (err u18))
        )
      )
      (begin
        (map-set claims
          { pool-id: pool-, claim-id: claim-id }
          (merge claim-data {
            status: "rejected",
            paid: false
          })
        )
      )
    )
    
    (ok true)
  )
)
(define-public (distribute-profits (pol-id uint))
  (let
    ((pool (unwrap! (get-pool pol-id) (err u2))))
    
    (asserts! (get active pool) (err u3))
    (asserts! (is-eq tx-sender (get creator pool)) (err u16))
    
    (let
      ((profit-amount (* (get total-funds pool) (var-get profit-sharing-percentage) (/ u1 u100))))
      
      (map-set pools
        { id: pol-id }
        (merge pool {
          total-funds: (- (get total-funds pool) profit-amount)
        })
      )
      
      (as-contract (stx-transfer? profit-amount tx-sender (get creator pool)))
    )
  )
)

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u17))
    (var-set admin new-admin)
    (ok true)
  )
)

(define-public (set-min-contribution (new-min uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u17))
    (var-set min-contribution new-min)
    (ok true)
  )
)

(define-public (set-claim-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u17))
    (var-set claim-threshold new-threshold)
    (ok true)
  )
)

(define-public (set-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u17))
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (set-profit-sharing-percentage (new-percentage uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u17))
    (asserts! (<= new-percentage u20) (err u18))
    (var-set profit-sharing-percentage new-percentage)
    (ok true)
  )
)



(define-data-var pool-paused bool false)

(define-read-only (is-pool-paused)
  (var-get pool-paused)
)

(define-public (pause-pool)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u19))
    (var-set pool-paused true)
    (ok true)
  )
)

(define-public (unpause-pool)
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u19))
    (var-set pool-paused false)
    (ok true)
  )
)


(define-map pool-statistics
  { pool-id: uint }
  {
    total-claims-filed: uint,
    total-claims-approved: uint,
    total-amount-paid: uint,
    average-processing-time: uint,
    last-activity-block: uint
  }
)

(define-public (initialize-pool-stats (pool-id-param uint))
  (begin
    (asserts! (is-some (get-pool pool-id-param)) (err u21))
    (map-set pool-statistics
      { pool-id: pool-id-param }
      {
        total-claims-filed: u0,
        total-claims-approved: u0,
        total-amount-paid: u0,
        average-processing-time: u0,
        last-activity-block: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-read-only (get-pool-statistics (pool-id-param uint))
  (map-get? pool-statistics { pool-id: pool-id-param })
)