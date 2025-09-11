(define-data-var pool-id uint u0)
(define-data-var admin principal tx-sender)
(define-data-var min-contribution uint u1000000)
(define-data-var claim-threshold uint u500000)
(define-data-var voting-period uint u144)
(define-data-var profit-sharing-percentage uint u5)


(define-data-var risk-assessment-enabled bool true)
(define-data-var base-risk-score uint u100)
(define-data-var max-risk-multiplier uint u300)
(define-data-var min-risk-multiplier uint u50)
(define-data-var loyalty-bonus-threshold uint u12)
(define-data-var loyalty-discount-percentage uint u15)

(define-private (min-value (a uint) (b uint))
  (if (< a b) a b)
)

(define-private (max-value (a uint) (b uint))
  (if (> a b) a b)
)

(define-map member-risk-profile
  { pool-id: uint, member: principal }
  {
    risk-score: uint,
    claims-ratio: uint,
    participation-score: uint,
    loyalty-months: uint,
    last-assessment-block: uint,
    premium-multiplier: uint,
    warnings-issued: uint,
    consecutive-payments: uint
  }
)

(define-map pool-risk-metrics
  { pool-id: uint }
  {
    average-risk-score: uint,
    total-risk-adjustments: uint,
    pool-stability-score: uint,
    risk-distribution: (list 5 uint),
    last-rebalance-block: uint,
    high-risk-member-count: uint,
    low-risk-member-count: uint
  }
)

(define-map risk-adjustment-history
  { pool-id: uint, member: principal, adjustment-id: uint }
  {
    old-multiplier: uint,
    new-multiplier: uint,
    reason: (string-ascii 50),
    adjustment-block: uint,
    automated: bool
  }
)

(define-map member-adjustment-counter
  { pool-id: uint, member: principal }
  { counter: uint }
)

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

;; Dispute Resolution System - Unique peer-to-peer insurance conflict management
(define-data-var dispute-counter uint u0)
(define-data-var arbitration-period uint u288) ;; 2 days in blocks
(define-data-var min-arbitrator-stake uint u500000)
(define-data-var arbitrator-reward-percentage uint u3)
(define-data-var dispute-fee uint u100000)

(define-map disputes
  { dispute-id: uint }
  {
    pool-id: uint,
    initiator: principal,
    respondent: principal,
    dispute-type: (string-ascii 30),
    description: (string-ascii 300),
    amount-disputed: uint,
    filed-block: uint,
    status: (string-ascii 20),
    arbitrator: (optional principal),
    arbitrator-deadline: uint,
    evidence-count: uint,
    community-votes-for: uint,
    community-votes-against: uint,
    resolution: (string-ascii 200),
    appeal-count: uint
  }
)

(define-map dispute-evidence
  { dispute-id: uint, evidence-id: uint }
  {
    submitter: principal,
    evidence-type: (string-ascii 20),
    description: (string-ascii 250),
    submission-block: uint,
    verified: bool
  }
)

(define-map arbitrators
  { pool-id: uint, arbitrator: principal }
  {
    stake-amount: uint,
    cases-handled: uint,
    success-rate: uint,
    registration-block: uint,
    active: bool,
    disputes-assigned: uint
  }
)

(define-map dispute-votes
  { dispute-id: uint, voter: principal }
  {
    vote: bool,
    voting-power: uint,
    vote-block: uint
  }
)

(define-map evidence-counter
  { dispute-id: uint }
  { counter: uint }
)

;; Read-only functions for dispute system
(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-arbitrator-info (pool-identifier uint) (arbitrator principal))
  (map-get? arbitrators { pool-id: pool-identifier, arbitrator: arbitrator })
)

(define-read-only (get-dispute-evidence (dispute-id uint) (evidence-id uint))
  (map-get? dispute-evidence { dispute-id: dispute-id, evidence-id: evidence-id })
)

(define-read-only (get-dispute-vote (dispute-id uint) (voter principal))
  (map-get? dispute-votes { dispute-id: dispute-id, voter: voter })
)

;; Register as arbitrator - community members can mediate disputes
(define-public (register-arbitrator (pool-identifier uint) (stake-amount uint))
  (let
    ((pool (unwrap! (get-pool pool-identifier) (err u50)))
     (member-data (unwrap! (get-pool-member pool-identifier tx-sender) (err u51))))
    
    (asserts! (get active pool) (err u52))
    (asserts! (get active member-data) (err u53))
    (asserts! (>= stake-amount (var-get min-arbitrator-stake)) (err u54))
    (asserts! (is-none (get-arbitrator-info pool-identifier tx-sender)) (err u55))
    
    ;; Transfer stake amount to contract
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    
    (map-set arbitrators
      { pool-id: pool-identifier, arbitrator: tx-sender }
      {
        stake-amount: stake-amount,
        cases-handled: u0,
        success-rate: u100,
        registration-block: stacks-block-height,
        active: true,
        disputes-assigned: u0
      }
    )
    
    (ok true)
  )
)

;; File a dispute - members can challenge decisions or conflicts
(define-public (file-dispute 
    (pool-identifier uint) 
    (respondent principal) 
    (dispute-type (string-ascii 30)) 
    (description (string-ascii 300)) 
    (amount-disputed uint))
  (let
    ((pool (unwrap! (get-pool pool-identifier) (err u50)))
     (member-data (unwrap! (get-pool-member pool-identifier tx-sender) (err u51)))
     (new-dispute-id (var-get dispute-counter)))
    
    (asserts! (get active pool) (err u52))
    (asserts! (get active member-data) (err u53))
    (asserts! (not (is-eq tx-sender respondent)) (err u56))
    
    ;; Pay dispute filing fee
    (try! (stx-transfer? (var-get dispute-fee) tx-sender (as-contract tx-sender)))
    
    (map-set disputes
      { dispute-id: new-dispute-id }
      {
        pool-id: pool-identifier,
        initiator: tx-sender,
        respondent: respondent,
        dispute-type: dispute-type,
        description: description,
        amount-disputed: amount-disputed,
        filed-block: stacks-block-height,
        status: "pending",
        arbitrator: none,
        arbitrator-deadline: u0,
        evidence-count: u0,
        community-votes-for: u0,
        community-votes-against: u0,
        resolution: "",
        appeal-count: u0
      }
    )
    
    (map-set evidence-counter
      { dispute-id: new-dispute-id }
      { counter: u0 }
    )
    
    (var-set dispute-counter (+ new-dispute-id u1))
    (ok new-dispute-id)
  )
)

;; Assign arbitrator to dispute - automated selection based on availability
(define-public (assign-arbitrator (dispute-id uint) (arbitrator principal))
  (let
    ((dispute-data (unwrap! (get-dispute dispute-id) (err u57)))
     (arbitrator-info (unwrap! (get-arbitrator-info (get pool-id dispute-data) arbitrator) (err u58))))
    
    (asserts! (is-eq (get status dispute-data) "pending") (err u59))
    (asserts! (get active arbitrator-info) (err u60))
    (asserts! (not (is-eq arbitrator (get initiator dispute-data))) (err u61))
    (asserts! (not (is-eq arbitrator (get respondent dispute-data))) (err u61))
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data {
        arbitrator: (some arbitrator),
        arbitrator-deadline: (+ stacks-block-height (var-get arbitration-period)),
        status: "arbitration"
      })
    )
    
    (map-set arbitrators
      { pool-id: (get pool-id dispute-data), arbitrator: arbitrator }
      (merge arbitrator-info {
        disputes-assigned: (+ (get disputes-assigned arbitrator-info) u1)
      })
    )
    
    (ok true)
  )
)

;; Submit evidence for dispute - both parties can provide supporting materials
(define-public (submit-evidence 
    (dispute-id uint) 
    (evidence-type (string-ascii 20)) 
    (description (string-ascii 250)))
  (let
    ((dispute-data (unwrap! (get-dispute dispute-id) (err u57)))
     (evidence-counter-data (unwrap! (map-get? evidence-counter { dispute-id: dispute-id }) (err u62))))
    
    (asserts! (is-eq (get status dispute-data) "arbitration") (err u63))
    (asserts! (or (is-eq tx-sender (get initiator dispute-data))
                  (is-eq tx-sender (get respondent dispute-data))) (err u64))
    
    (let
      ((evidence-id (get counter evidence-counter-data)))
      
      (map-set dispute-evidence
        { dispute-id: dispute-id, evidence-id: evidence-id }
        {
          submitter: tx-sender,
          evidence-type: evidence-type,
          description: description,
          submission-block: stacks-block-height,
          verified: false
        }
      )
      
      (map-set evidence-counter
        { dispute-id: dispute-id }
        { counter: (+ evidence-id u1) }
      )
      
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute-data {
          evidence-count: (+ (get evidence-count dispute-data) u1)
        })
      )
      
      (ok evidence-id)
    )
  )
)

;; Arbitrator makes initial ruling on dispute
(define-public (arbitrator-ruling (dispute-id uint) (resolution (string-ascii 200)) (favor-initiator bool))
  (let
    ((dispute-data (unwrap! (get-dispute dispute-id) (err u57))))
    
    (asserts! (is-eq (get status dispute-data) "arbitration") (err u63))
    (asserts! (is-eq (some tx-sender) (get arbitrator dispute-data)) (err u65))
    (asserts! (<= stacks-block-height (get arbitrator-deadline dispute-data)) (err u66))
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data {
        resolution: resolution,
        status: "community-review"
      })
    )
    
    ;; Update arbitrator stats
    (let
      ((arbitrator-info (unwrap! (get-arbitrator-info (get pool-id dispute-data) tx-sender) (err u58))))
      (map-set arbitrators
        { pool-id: (get pool-id dispute-data), arbitrator: tx-sender }
        (merge arbitrator-info {
          cases-handled: (+ (get cases-handled arbitrator-info) u1)
        })
      )
    )
    
    (ok true)
  )
)

;; Community voting on arbitrator decisions - democratic oversight
(define-public (vote-on-dispute (dispute-id uint) (support-ruling bool))
  (let
    ((dispute-data (unwrap! (get-dispute dispute-id) (err u57)))
     (member-data (unwrap! (get-pool-member (get pool-id dispute-data) tx-sender) (err u51))))
    
    (asserts! (is-eq (get status dispute-data) "community-review") (err u67))
    (asserts! (get active member-data) (err u53))
    (asserts! (not (is-eq tx-sender (get initiator dispute-data))) (err u68))
    (asserts! (not (is-eq tx-sender (get respondent dispute-data))) (err u68))
    (asserts! (is-none (get-dispute-vote dispute-id tx-sender)) (err u69))
    
    ;; Calculate voting power based on contribution and membership duration
    (let
      ((voting-power (+ (/ (get contribution member-data) u100000)
                       (/ (- stacks-block-height (get joined-block member-data)) u1440))))
      
      (map-set dispute-votes
        { dispute-id: dispute-id, voter: tx-sender }
        {
          vote: support-ruling,
          voting-power: voting-power,
          vote-block: stacks-block-height
        }
      )
      
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute-data {
          community-votes-for: (+ (get community-votes-for dispute-data) 
                                 (if support-ruling voting-power u0)),
          community-votes-against: (+ (get community-votes-against dispute-data) 
                                     (if support-ruling u0 voting-power))
        })
      )
      
      (ok true)
    )
  )
)

;; Finalize dispute after community review period
(define-public (finalize-dispute (dispute-id uint))
  (let
    ((dispute-data (unwrap! (get-dispute dispute-id) (err u57))))
    
    (asserts! (is-eq (get status dispute-data) "community-review") (err u67))
    (asserts! (>= (- stacks-block-height (get filed-block dispute-data)) u432) (err u70)) ;; 3 day minimum
    
    (let
      ((ruling-upheld (> (get community-votes-for dispute-data) (get community-votes-against dispute-data))))
      
      (map-set disputes
        { dispute-id: dispute-id }
        (merge dispute-data {
          status: (if ruling-upheld "resolved-upheld" "resolved-overturned")
        })
      )
      
      ;; Reward arbitrator if ruling was upheld
      (if ruling-upheld
        (match (get arbitrator dispute-data)
          arbitrator-principal
          (let
            ((reward-amount (/ (* (get amount-disputed dispute-data) (var-get arbitrator-reward-percentage)) u100)))
            (as-contract (stx-transfer? reward-amount tx-sender arbitrator-principal))
          )
          (ok true)
        )
        (ok true)
      )
    )
  )
)

;; File appeal for dispute resolution - second chance for complex cases
(define-public (file-appeal (dispute-id uint) (appeal-reason (string-ascii 200)))
  (let
    ((dispute-data (unwrap! (get-dispute dispute-id) (err u57))))
    
    (asserts! (or (is-eq (get status dispute-data) "resolved-upheld")
                  (is-eq (get status dispute-data) "resolved-overturned")) (err u71))
    (asserts! (< (get appeal-count dispute-data) u2) (err u72)) ;; Max 2 appeals
    (asserts! (or (is-eq tx-sender (get initiator dispute-data))
                  (is-eq tx-sender (get respondent dispute-data))) (err u64))
    
    ;; Higher fee for appeals
    (try! (stx-transfer? (* (var-get dispute-fee) u2) tx-sender (as-contract tx-sender)))
    
    (map-set disputes
      { dispute-id: dispute-id }
      (merge dispute-data {
        status: "appeal-pending",
        appeal-count: (+ (get appeal-count dispute-data) u1),
        arbitrator: none
      })
    )
    
    (ok true)
  )
)

;; Admin functions for dispute system management
(define-public (update-dispute-parameters 
    (new-arbitration-period uint) 
    (new-min-stake uint) 
    (new-dispute-fee uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u17))
    (asserts! (> new-arbitration-period u144) (err u73)) ;; Minimum 1 day
    
    (var-set arbitration-period new-arbitration-period)
    (var-set min-arbitrator-stake new-min-stake)
    (var-set dispute-fee new-dispute-fee)
    
    (ok true)
  )
)


(define-read-only (get-member-risk-profile (pid uint) (member principal))
  (map-get? member-risk-profile { pool-id: pid, member: member })
)

(define-read-only (get-pool-risk-metrics (pid uint))
  (map-get? pool-risk-metrics { pool-id: pid })
)

(define-read-only (get-risk-adjustment-history (pid uint) (member principal) (adjustment-id uint))
  (map-get? risk-adjustment-history { pool-id: pid, member: member, adjustment-id: adjustment-id })
)

(define-read-only (calculate-risk-based-premium (pid uint) (member principal))
  (match (get-pool-premium-config pid)
    config
    (match (get-member-risk-profile pid member)
      risk-profile
      (let
        ((base-premium (get premium-amount config))
         (risk-multiplier (get premium-multiplier risk-profile))
         (loyalty-discount (if (>= (get loyalty-months risk-profile) (var-get loyalty-bonus-threshold))
                             (var-get loyalty-discount-percentage)
                             u0)))
        (let
          ((adjusted-premium (/ (* base-premium risk-multiplier) u100))
           (final-premium (- adjusted-premium (/ (* adjusted-premium loyalty-discount) u100))))
          (ok final-premium)
        )
      )
      (ok (get premium-amount config))
    )
    (err u40)
  )
)

(define-public (initialize-member-risk-profile (pid uint) (member principal))
  (let
    ((pool (unwrap! (get-pool pid) (err u2)))
     (member-data (unwrap! (get-pool-member pid member) (err u6))))
    
    (asserts! (get active member-data) (err u7))
    (asserts! (is-none (get-member-risk-profile pid member)) (err u41))
    
    (map-set member-risk-profile
      { pool-id: pid, member: member }
      {
        risk-score: (var-get base-risk-score),
        claims-ratio: u0,
        participation-score: u100,
        loyalty-months: u0,
        last-assessment-block: stacks-block-height,
        premium-multiplier: u100,
        warnings-issued: u0,
        consecutive-payments: u0
      }
    )
    
    (map-set member-adjustment-counter
      { pool-id: pid, member: member }
      { counter: u0 }
    )
    
    (ok true)
  )
)

(define-public (assess-member-risk (pid uint) (member principal))
  (let
    ((pool (unwrap! (get-pool pid) (err u2)))
     (member-data (unwrap! (get-pool-member pid member) (err u6)))
     (risk-profile (unwrap! (get-member-risk-profile pid member) (err u42)))
     (premium-status (get-member-premium-status pid member)))
    
    (asserts! (var-get risk-assessment-enabled) (err u43))
    (asserts! (get active member-data) (err u7))
    
    (let
      ((claims-filed (get claims-filed member-data))
       (claims-approved (get claims-approved member-data))
       (claims-ratio (if (> claims-filed u0)
                       (/ (* claims-approved u100) claims-filed)
                       u0))
       (participation-score (calculate-participation-score pid member))
       (loyalty-months (calculate-loyalty-months pid member))
       (consecutive-payments (match premium-status
                               status (get payments-made status)
                               u0)))
      
      (let
        ((new-risk-score (calculate-risk-score claims-ratio participation-score loyalty-months consecutive-payments))
         (new-multiplier (calculate-premium-multiplier new-risk-score)))
        
        (map-set member-risk-profile
          { pool-id: pid, member: member }
          (merge risk-profile {
            risk-score: new-risk-score,
            claims-ratio: claims-ratio,
            participation-score: participation-score,
            loyalty-months: loyalty-months,
            last-assessment-block: stacks-block-height,
            premium-multiplier: new-multiplier,
            consecutive-payments: consecutive-payments
          })
        )
        
        (record-risk-adjustment pid member (get premium-multiplier risk-profile) new-multiplier "automated-assessment")
      )
    )
  )
)

(define-private (calculate-participation-score (pid uint) (member principal))
  (let
    ((pool (unwrap-panic (get-pool pid)))
     (member-data (unwrap-panic (get-pool-member pid member)))
     (blocks-since-joined (- stacks-block-height (get joined-block member-data)))
     (expected-participation (/ blocks-since-joined u144)))
    
    (if (> expected-participation u0)
      (min-value u150 (+ u50 (/ (* u50 u1) expected-participation)))
      u100
    )
  )
)

(define-private (calculate-loyalty-months (pid uint) (member principal))
  (let
    ((member-data (unwrap-panic (get-pool-member pid member)))
     (blocks-since-joined (- stacks-block-height (get joined-block member-data)))
     (months (/ blocks-since-joined u4320)))
    months
  )
)

(define-private (calculate-risk-score (claims-ratio uint) (participation-score uint) (loyalty-months uint) (consecutive-payments uint))
  (let
    ((base-score (var-get base-risk-score))
     (claims-impact (if (> claims-ratio u50) (+ u20 (/ claims-ratio u5)) u0))
     (participation-bonus (if (> participation-score u120) u10 u0))
     (loyalty-bonus (min-value u15 (/ loyalty-months u2)))
     (payment-bonus (min-value u10 (/ consecutive-payments u3))))
    
    (let
      ((adjusted-score (+ base-score claims-impact)))
      (max-value u20 (- adjusted-score (+ participation-bonus loyalty-bonus payment-bonus)))
    )
  )
)

(define-private (calculate-premium-multiplier (risk-score uint))
  (let
    ((base-multiplier u100)
     (risk-factor (if (> risk-score u100)
                    (min-value (var-get max-risk-multiplier) (+ u100 (/ (* (- risk-score u100) u2) u1)))
                    (max-value (var-get min-risk-multiplier) (- u100 (/ (* (- u100 risk-score) u1) u2))))))
    risk-factor
  )
)

(define-private (record-risk-adjustment (pid uint) (member principal) (old-multiplier uint) (new-multiplier uint) (reason (string-ascii 50)))
  (let
    ((adjustment-counter (unwrap! (map-get? member-adjustment-counter { pool-id: pid, member: member }) (err u44))))
    
    (let
      ((adjustment-id (get counter adjustment-counter)))
      
      (map-set risk-adjustment-history
        { pool-id: pid, member: member, adjustment-id: adjustment-id }
        {
          old-multiplier: old-multiplier,
          new-multiplier: new-multiplier,
          reason: reason,
          adjustment-block: stacks-block-height,
          automated: true
        }
      )
      
      (map-set member-adjustment-counter
        { pool-id: pid, member: member }
        { counter: (+ adjustment-id u1) }
      )
      
      (ok true)
    )
  )
)

(define-public (bulk-assess-member-risks (pid uint) (members (list 20 principal)))
  (let
    ((pool (unwrap! (get-pool pid) (err u2))))
    
    (asserts! (is-eq tx-sender (get creator pool)) (err u16))
    (asserts! (var-get risk-assessment-enabled) (err u43))
    
    (ok (map assess-member-risk-helper
         (map create-assessment-tuple members)))
  )
)

(define-private (create-assessment-tuple (member principal))
  { pid: u0, member: member }
)

(define-private (assess-member-risk-helper (data { pid: uint, member: principal }))
  (assess-member-risk (get pid data) (get member data))
)

(define-public (rebalance-pool-risk (pid uint))
  (let
    ((pool (unwrap! (get-pool pid) (err u2)))
     (current-metrics (get-pool-risk-metrics pid)))
    
    (asserts! (is-eq tx-sender (get creator pool)) (err u16))
    (asserts! (var-get risk-assessment-enabled) (err u43))
    
    (let
      ((stability-score (calculate-pool-stability-score pid))
       (high-risk-count (count-high-risk-members pid))
       (low-risk-count (count-low-risk-members pid)))
      
      (map-set pool-risk-metrics
        { pool-id: pid }
        {
          average-risk-score: (calculate-average-risk-score pid),
          total-risk-adjustments: (match current-metrics
                                    metrics (+ (get total-risk-adjustments metrics) u1)
                                    u1),
          pool-stability-score: stability-score,
          risk-distribution: (list u0 u0 u0 u0 u0),
          last-rebalance-block: stacks-block-height,
          high-risk-member-count: high-risk-count,
          low-risk-member-count: low-risk-count
        }
      )
      
      (ok true)
    )
  )
)

(define-private (calculate-pool-stability-score (pid uint))
  (let
    ((pool (unwrap-panic (get-pool pid))))
    
    (let
      ((member-count (get member-count pool))
       (total-funds (get total-funds pool))
       (base-stability (if (> member-count u10) u100 (* member-count u10))))
      
      (min-value u150 (+ base-stability (/ total-funds u1000000)))
    )
  )
)

(define-private (calculate-average-risk-score (pid uint))
  u100
)

(define-private (count-high-risk-members (pid uint))
  u0
)

(define-private (count-low-risk-members (pid uint))
  u0
)

(define-public (issue-risk-warning (pid uint) (member principal) (warning-reason (string-ascii 100)))
  (let
    ((pool (unwrap! (get-pool pid) (err u2)))
     (risk-profile (unwrap! (get-member-risk-profile pid member) (err u42))))
    
    (asserts! (is-eq tx-sender (get creator pool)) (err u16))
    
    (let
      ((new-warning-count (+ (get warnings-issued risk-profile) u1)))
      
      (map-set member-risk-profile
        { pool-id: pid, member: member }
        (merge risk-profile {
          warnings-issued: new-warning-count,
          premium-multiplier: (min-value (var-get max-risk-multiplier) 
                                 (+ (get premium-multiplier risk-profile) u25))
        })
      )
      
      (record-risk-adjustment pid member 
                             (get premium-multiplier risk-profile) 
                             (get premium-multiplier risk-profile) 
                             "warning-issued")
    )
  )
)

(define-public (reward-good-behavior (pid uint) (member principal))
  (let
    ((pool (unwrap! (get-pool pid) (err u2)))
     (risk-profile (unwrap! (get-member-risk-profile pid member) (err u42))))
    
    (asserts! (is-eq tx-sender (get creator pool)) (err u16))
    
    (let
      ((current-multiplier (get premium-multiplier risk-profile))
       (new-multiplier (max-value (var-get min-risk-multiplier) (- current-multiplier u15))))
      
      (map-set member-risk-profile
        { pool-id: pid, member: member }
        (merge risk-profile {
          premium-multiplier: new-multiplier,
          participation-score: (min-value u150 (+ (get participation-score risk-profile) u10))
        })
      )
      
      (record-risk-adjustment pid member current-multiplier new-multiplier "good-behavior-reward")
    )
  )
)

(define-public (toggle-risk-assessment (enabled bool))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u17))
    (var-set risk-assessment-enabled enabled)
    (ok true)
  )
)

(define-public (update-risk-parameters (new-base-score uint) (new-max-multiplier uint) (new-min-multiplier uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err u17))
    (asserts! (> new-base-score u0) (err u45))
    (asserts! (> new-max-multiplier new-min-multiplier) (err u46))
    
    (var-set base-risk-score new-base-score)
    (var-set max-risk-multiplier new-max-multiplier)
    (var-set min-risk-multiplier new-min-multiplier)
    
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


(define-data-var default-premium-period uint u4320)
(define-data-var grace-period-blocks uint u1440)
(define-data-var late-payment-penalty-percentage uint u10)

(define-map pool-premium-config
  { pool-id: uint }
  {
    premium-amount: uint,
    premium-period-blocks: uint,
    auto-collection-enabled: bool,
    penalty-rate: uint
  }
)

(define-map member-premium-status
  { pool-id: uint, member: principal }
  {
    last-payment-block: uint,
    next-due-block: uint,
    payments-made: uint,
    total-penalties: uint,
    coverage-active: bool,
    grace-period-end: uint
  }
)

(define-map premium-payments
  { pool-id: uint, member: principal, payment-id: uint }
  {
    amount: uint,
    payment-block: uint,
    period-covered: uint,
    penalty-amount: uint
  }
)

(define-map member-payment-counter
  { pool-id: uint, member: principal }
  { counter: uint }
)

(define-read-only (get-pool-premium-config (pid uint))
  (map-get? pool-premium-config { pool-id: pid })
)

(define-read-only (get-member-premium-status (pid uint) (member principal))
  (map-get? member-premium-status { pool-id: pid, member: member })
)

(define-read-only (get-premium-payment (pid uint) (member principal) (payment-id uint))
  (map-get? premium-payments { pool-id: pid, member: member, payment-id: payment-id })
)

(define-read-only (is-coverage-active (pid uint) (member principal))
  (match (get-member-premium-status pid member)
    premium-status (get coverage-active premium-status)
    false
  )
)

(define-read-only (calculate-premium-due (pid uint) (member principal))
  (match (get-pool-premium-config pid)
    config
    (match (get-member-premium-status pid member)
      status
      (let
        ((blocks-overdue (if (> stacks-block-height (get next-due-block status))
                           (- stacks-block-height (get next-due-block status))
                           u0))
         (base-premium (get premium-amount config))
         (penalty (if (> blocks-overdue u0)
                    (/ (* base-premium (get penalty-rate config)) u100)
                    u0)))
        (ok (+ base-premium penalty))
      )
      (ok (get premium-amount config))
    )
    (err u30)
  )
)

(define-public (setup-pool-premiums (pid uint) (premium-amount uint) (premium-period-blocks uint))
  (let
    ((pool (unwrap! (get-pool pid) (err u2))))
    
    (asserts! (is-eq tx-sender (get creator pool)) (err u16))
    (asserts! (> premium-amount u0) (err u31))
    (asserts! (> premium-period-blocks u0) (err u32))
    
    (map-set pool-premium-config
      { pool-id: pid }
      {
        premium-amount: premium-amount,
        premium-period-blocks: premium-period-blocks,
        auto-collection-enabled: true,
        penalty-rate: (var-get late-payment-penalty-percentage)
      }
    )
    
    (ok true)
  )
)

(define-public (initialize-member-premiums (pid uint) (member principal))
  (let
    ((pool (unwrap! (get-pool pid) (err u2)))
     (config (unwrap! (get-pool-premium-config pid) (err u33)))
     (member-data (unwrap! (get-pool-member pid member) (err u6))))
    
    (asserts! (get active member-data) (err u7))
    
    (map-set member-premium-status
      { pool-id: pid, member: member }
      {
        last-payment-block: (get joined-block member-data),
        next-due-block: (+ (get joined-block member-data) (get premium-period-blocks config)),
        payments-made: u0,
        total-penalties: u0,
        coverage-active: true,
        grace-period-end: u0
      }
    )
    
    (map-set member-payment-counter
      { pool-id: pid, member: member }
      { counter: u0 }
    )
    
    (ok true)
  )
)

(define-public (pay-premium (pid uint))
  (let
    ((pool (unwrap! (get-pool pid) (err u2)))
     (config (unwrap! (get-pool-premium-config pid) (err u33)))
     (premium-status (unwrap! (get-member-premium-status pid tx-sender) (err u34)))
     (payment-counter (unwrap! (map-get? member-payment-counter { pool-id: pid, member: tx-sender }) (err u35)))
     (premium-due (unwrap! (calculate-premium-due pid tx-sender) (err u36))))
    
    (asserts! (not (var-get pool-paused)) (err u20))
    (asserts! (get active pool) (err u3))
    
    (let
      ((current-payment-id (get counter payment-counter))
       (blocks-overdue (if (> stacks-block-height (get next-due-block premium-status))
                         (- stacks-block-height (get next-due-block premium-status))
                         u0))
       (penalty-amount (if (> blocks-overdue u0)
                         (/ (* (get premium-amount config) (get penalty-rate config)) u100)
                         u0))
       (new-next-due (+ stacks-block-height (get premium-period-blocks config))))
      
      (map-set premium-payments
        { pool-id: pid, member: tx-sender, payment-id: current-payment-id }
        {
          amount: premium-due,
          payment-block: stacks-block-height,
          period-covered: (get premium-period-blocks config),
          penalty-amount: penalty-amount
        }
      )
      
      (map-set member-premium-status
        { pool-id: pid, member: tx-sender }
        (merge premium-status {
          last-payment-block: stacks-block-height,
          next-due-block: new-next-due,
          payments-made: (+ (get payments-made premium-status) u1),
          total-penalties: (+ (get total-penalties premium-status) penalty-amount),
          coverage-active: true,
          grace-period-end: u0
        })
      )
      
      (map-set member-payment-counter
        { pool-id: pid, member: tx-sender }
        { counter: (+ current-payment-id u1) }
      )
      
      (map-set pools
        { id: pid }
        (merge pool {
          total-funds: (+ (get total-funds pool) premium-due)
        })
      )
      
      (stx-transfer? premium-due tx-sender (as-contract tx-sender))
    )
  )
)

(define-public (suspend-coverage-for-non-payment (pid uint) (member principal))
  (let
    ((config (unwrap! (get-pool-premium-config pid) (err u33)))
     (premium-status (unwrap! (get-member-premium-status pid member) (err u34))))
    
    (asserts! (> stacks-block-height (get next-due-block premium-status)) (err u37))
    (asserts! (get coverage-active premium-status) (err u38))
    
    (let
      ((grace-end (+ (get next-due-block premium-status) (var-get grace-period-blocks))))
      
      (if (> stacks-block-height grace-end)
        (map-set member-premium-status
          { pool-id: pid, member: member }
          (merge premium-status {
            coverage-active: false,
            grace-period-end: u0
          })
        )
        (map-set member-premium-status
          { pool-id: pid, member: member }
          (merge premium-status {
            grace-period-end: grace-end
          })
        )
      )
    )
    
    (ok true)
  )
)

(define-public (bulk-suspend-overdue-members (pid uint) (members (list 50 principal)))
  (let
    ((pool (unwrap! (get-pool pid) (err u2))))
    
    (asserts! (is-eq tx-sender (get creator pool)) (err u16))
    
    (ok (map suspend-coverage-for-non-payment-helper 
         (map create-pool-member-tuple members)))
  )
)

(define-private (create-pool-member-tuple (member principal))
  { pid: u0, member: member }
)

(define-private (suspend-coverage-for-non-payment-helper (data { pid: uint, member: principal }))
  (suspend-coverage-for-non-payment (get pid data) (get member data))
)

(define-read-only (get-overdue-members (pid uint))
  (ok pid)
)

(define-public (update-premium-config (pid uint) (new-premium-amount uint) (new-penalty-rate uint))
  (let
    ((pool (unwrap! (get-pool pid) (err u2)))
     (config (unwrap! (get-pool-premium-config pid) (err u33))))
    
    (asserts! (is-eq tx-sender (get creator pool)) (err u16))
    (asserts! (> new-premium-amount u0) (err u31))
    (asserts! (<= new-penalty-rate u50) (err u39))
    
    (map-set pool-premium-config
      { pool-id: pid }
      (merge config {
        premium-amount: new-premium-amount,
        penalty-rate: new-penalty-rate
      })
    )
    
    (ok true)
  )
)

;; =====================================
;; EMERGENCY FUND RESERVE SYSTEM
;; =====================================

;; Emergency Fund Constants
(define-constant ERR_EMERGENCY_NOT_DECLARED (err u500))
(define-constant ERR_EMERGENCY_ALREADY_ACTIVE (err u501))
(define-constant ERR_INSUFFICIENT_RESERVE (err u502))
(define-constant ERR_INVALID_RESERVE_PERCENTAGE (err u503))
(define-constant ERR_EMERGENCY_CLAIM_LIMIT_EXCEEDED (err u504))
(define-constant ERR_RESERVE_LOCKED (err u505))

;; Emergency system variables
(define-data-var emergency-reserve-percentage uint u15) ;; 15% of contributions go to emergency fund
(define-data-var emergency-claim-multiplier uint u200) ;; 2x normal claim limit during emergencies
(define-data-var emergency-duration-blocks uint u1440) ;; 1 day emergency period
(define-data-var min-reserve-threshold uint u1000000) ;; Minimum reserve before emergency claims

;; Emergency fund tracking
(define-map pool-emergency-reserves
    { pool-id: uint }
    {
        reserve-amount: uint,
        target-reserve: uint,
        reserve-percentage: uint,
        last-contribution-block: uint,
        emergency-active: bool,
        emergency-start-block: uint,
        emergency-end-block: uint,
        emergency-claims-paid: uint,
        total-emergency-claims: uint
    }
)

;; Emergency events log
(define-map emergency-events
    { pool-id: uint, event-id: uint }
    {
        event-type: (string-ascii 30),
        description: (string-ascii 200),
        declared-by: principal,
        declared-at: uint,
        claims-processed: uint,
        total-payout: uint,
        resolved: bool
    }
)

;; Emergency claims tracking
(define-map emergency-claims
    { pool-id: uint, claim-id: uint }
    {
        is-emergency-claim: bool,
        auto-approved: bool,
        emergency-event-id: uint,
        expedited-payout: uint
    }
)

;; Emergency event counter
(define-map pool-emergency-counter
    { pool-id: uint }
    { counter: uint }
)

;; Initialize emergency reserve for a pool
(define-public (initialize-emergency-reserve (pool-identifier uint) (reserve-percentage uint))
    (let (
        (pool (unwrap! (get-pool pool-identifier) (err u2)))
        (current-reserve (map-get? pool-emergency-reserves { pool-id: pool-identifier }))
    )
        (asserts! (is-eq tx-sender (get creator pool)) (err u16))
        (asserts! (get active pool) (err u3))
        (asserts! (and (>= reserve-percentage u5) (<= reserve-percentage u30)) ERR_INVALID_RESERVE_PERCENTAGE)
        (asserts! (is-none current-reserve) (err u41))
        
        ;; Calculate target reserve based on pool size
        (let (
            (target-reserve (/ (* (get total-funds pool) reserve-percentage) u100))
        )
            (map-set pool-emergency-reserves
                { pool-id: pool-identifier }
                {
                    reserve-amount: u0,
                    target-reserve: target-reserve,
                    reserve-percentage: reserve-percentage,
                    last-contribution-block: stacks-block-height,
                    emergency-active: false,
                    emergency-start-block: u0,
                    emergency-end-block: u0,
                    emergency-claims-paid: u0,
                    total-emergency-claims: u0
                }
            )
            
            (map-set pool-emergency-counter
                { pool-id: pool-identifier }
                { counter: u0 }
            )
            
            (ok true)
        )
    )
)

;; Contribute to emergency reserve (automatic when joining pool)
(define-public (contribute-to-emergency-reserve (pool-identifier uint) (contribution-amount uint))
    (let (
        (pool (unwrap! (get-pool pool-identifier) (err u2)))
        (reserve-data (unwrap! (map-get? pool-emergency-reserves { pool-id: pool-identifier }) ERR_EMERGENCY_NOT_DECLARED))
        (emergency-contribution (/ (* contribution-amount (get reserve-percentage reserve-data)) u100))
    )
        (asserts! (get active pool) (err u3))
        (asserts! (> contribution-amount u0) (err u4))
        
        ;; Transfer emergency contribution to reserves
        (map-set pool-emergency-reserves
            { pool-id: pool-identifier }
            (merge reserve-data {
                reserve-amount: (+ (get reserve-amount reserve-data) emergency-contribution),
                last-contribution-block: stacks-block-height
            })
        )
        
        (ok emergency-contribution)
    )
)

;; Declare emergency for rapid claim processing
(define-public (declare-emergency (pool-identifier uint) (event-type (string-ascii 30)) (description (string-ascii 200)))
    (let (
        (pool (unwrap! (get-pool pool-identifier) (err u2)))
        (reserve-data (unwrap! (map-get? pool-emergency-reserves { pool-id: pool-identifier }) ERR_EMERGENCY_NOT_DECLARED))
        (event-counter (unwrap! (map-get? pool-emergency-counter { pool-id: pool-identifier }) (err u8)))
        (current-block stacks-block-height)
    )
        (asserts! (is-eq tx-sender (get creator pool)) (err u16))
        (asserts! (get active pool) (err u3))
        (asserts! (not (get emergency-active reserve-data)) ERR_EMERGENCY_ALREADY_ACTIVE)
        (asserts! (>= (get reserve-amount reserve-data) (var-get min-reserve-threshold)) ERR_INSUFFICIENT_RESERVE)
        
        (let (
            (event-id (get counter event-counter))
            (emergency-end (+ current-block (var-get emergency-duration-blocks)))
        )
            ;; Update reserve status
            (map-set pool-emergency-reserves
                { pool-id: pool-identifier }
                (merge reserve-data {
                    emergency-active: true,
                    emergency-start-block: current-block,
                    emergency-end-block: emergency-end
                })
            )
            
            ;; Log emergency event
            (map-set emergency-events
                { pool-id: pool-identifier, event-id: event-id }
                {
                    event-type: event-type,
                    description: description,
                    declared-by: tx-sender,
                    declared-at: current-block,
                    claims-processed: u0,
                    total-payout: u0,
                    resolved: false
                }
            )
            
            ;; Update event counter
            (map-set pool-emergency-counter
                { pool-id: pool-identifier }
                { counter: (+ event-id u1) }
            )
            
            (ok event-id)
        )
    )
)

;; Process emergency claim with expedited approval
(define-public (process-emergency-claim (pool-identifier uint) (claim-id uint))
    (let (
        (pool (unwrap! (get-pool pool-identifier) (err u2)))
        (claim-data (unwrap! (get-claim pool-identifier claim-id) (err u10)))
        (reserve-data (unwrap! (map-get? pool-emergency-reserves { pool-id: pool-identifier }) ERR_EMERGENCY_NOT_DECLARED))
        (current-block stacks-block-height)
    )
        (asserts! (get active pool) (err u3))
        (asserts! (get emergency-active reserve-data) ERR_EMERGENCY_NOT_DECLARED)
        (asserts! (< current-block (get emergency-end-block reserve-data)) ERR_EMERGENCY_NOT_DECLARED)
        (asserts! (is-eq (get status claim-data) "pending") (err u11))
        
        ;; Check emergency claim limits
        (let (
            (max-emergency-amount (/ (* (get coverage-amount pool) (var-get emergency-claim-multiplier)) u100))
            (claim-amount (get amount claim-data))
        )
            (asserts! (<= claim-amount max-emergency-amount) ERR_EMERGENCY_CLAIM_LIMIT_EXCEEDED)
            (asserts! (>= (get reserve-amount reserve-data) claim-amount) ERR_INSUFFICIENT_RESERVE)
            
            ;; Auto-approve and pay emergency claim
            (map-set claims
                { pool-id: pool-identifier, claim-id: claim-id }
                (merge claim-data {
                    status: "emergency-approved",
                    paid: true
                })
            )
            
            ;; Mark as emergency claim
            (map-set emergency-claims
                { pool-id: pool-identifier, claim-id: claim-id }
                {
                    is-emergency-claim: true,
                    auto-approved: true,
                    emergency-event-id: (- (get counter (unwrap-panic (map-get? pool-emergency-counter { pool-id: pool-identifier }))) u1),
                    expedited-payout: claim-amount
                }
            )
            
            ;; Update reserves
            (map-set pool-emergency-reserves
                { pool-id: pool-identifier }
                (merge reserve-data {
                    reserve-amount: (- (get reserve-amount reserve-data) claim-amount),
                    emergency-claims-paid: (+ (get emergency-claims-paid reserve-data) u1),
                    total-emergency-claims: (+ (get total-emergency-claims reserve-data) claim-amount)
                })
            )
            
            ;; Process payout from emergency reserves
            (unwrap! (as-contract (stx-transfer? claim-amount tx-sender (get claimant claim-data))) (err u18))
            
            (ok true)
        )
    )
)

;; End emergency period
(define-public (resolve-emergency (pool-identifier uint))
    (let (
        (pool (unwrap! (get-pool pool-identifier) (err u2)))
        (reserve-data (unwrap! (map-get? pool-emergency-reserves { pool-id: pool-identifier }) ERR_EMERGENCY_NOT_DECLARED))
        (current-event-id (- (get counter (unwrap! (map-get? pool-emergency-counter { pool-id: pool-identifier }) (err u8))) u1))
        (current-block stacks-block-height)
    )
        (asserts! (is-eq tx-sender (get creator pool)) (err u16))
        (asserts! (get emergency-active reserve-data) ERR_EMERGENCY_NOT_DECLARED)
        (asserts! (> current-block (get emergency-end-block reserve-data)) (err u37))
        
        ;; End emergency period
        (map-set pool-emergency-reserves
            { pool-id: pool-identifier }
            (merge reserve-data {
                emergency-active: false,
                emergency-start-block: u0,
                emergency-end-block: u0
            })
        )
        
        ;; Mark event as resolved
        (map-set emergency-events
            { pool-id: pool-identifier, event-id: current-event-id }
            (merge (unwrap-panic (map-get? emergency-events { pool-id: pool-identifier, event-id: current-event-id })) {
                resolved: true
            })
        )
        
        (ok true)
    )
)

;; Read-only functions for emergency system
(define-read-only (get-emergency-reserve (pool-identifier uint))
    (map-get? pool-emergency-reserves { pool-id: pool-identifier })
)

(define-read-only (get-emergency-event (pool-identifier uint) (event-id uint))
    (map-get? emergency-events { pool-id: pool-identifier, event-id: event-id })
)

(define-read-only (get-emergency-claim-info (pool-identifier uint) (claim-id uint))
    (map-get? emergency-claims { pool-id: pool-identifier, claim-id: claim-id })
)

(define-read-only (is-emergency-active (pool-identifier uint))
    (match (map-get? pool-emergency-reserves { pool-id: pool-identifier })
        reserve-data
        (and 
            (get emergency-active reserve-data)
            (< stacks-block-height (get emergency-end-block reserve-data))
        )
        false
    )
)

(define-read-only (calculate-emergency-fund-health (pool-identifier uint))
    (match (map-get? pool-emergency-reserves { pool-id: pool-identifier })
        reserve-data
        (let (
            (current-reserve (get reserve-amount reserve-data))
            (target-reserve (get target-reserve reserve-data))
            (health-percentage (if (> target-reserve u0)
                                 (/ (* current-reserve u100) target-reserve)
                                 u0))
        )
            (some {
                health-percentage: health-percentage,
                reserve-status: (if (>= health-percentage u100) "healthy"
                               (if (>= health-percentage u75) "adequate"
                               (if (>= health-percentage u50) "low" "critical"))),
                current-amount: current-reserve,
                target-amount: target-reserve
            })
        )
        none
    )
)

