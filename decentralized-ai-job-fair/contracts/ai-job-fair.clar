;; Decentralized AI Job Fair - Blockchain job matching with escrow
;; Handles applications, matchmaking, and interview payment escrow

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-applied (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-status (err u104))

;; Data vars
(define-data-var job-nonce uint u0)
(define-data-var application-nonce uint u0)

;; Data maps
(define-map job-postings
    { job-id: uint }
    {
        company: principal,
        title: (string-ascii 100),
        bounty-amount: uint,
        status: (string-ascii 20),
        applications-count: uint
    }
)

(define-map applications
    { application-id: uint }
    {
        job-id: uint,
        applicant: principal,
        status: (string-ascii 20),
        interview-scheduled: bool
    }
)

(define-map escrows
    { application-id: uint }
    { amount: uint, released: bool }
)

;; Read-only functions
(define-read-only (get-job (job-id uint))
    (map-get? job-postings { job-id: job-id })
)

(define-read-only (get-application (application-id uint))
    (map-get? applications { application-id: application-id })
)

(define-read-only (get-escrow (application-id uint))
    (map-get? escrows { application-id: application-id })
)

(define-read-only (get-job-nonce)
    (ok (var-get job-nonce))
)

;; Public functions
;; #[allow(unchecked_data)]
(define-public (post-job (title (string-ascii 100)) (bounty-amount uint))
    (let
        (
            (new-job-id (+ (var-get job-nonce) u1))
        )
        (try! (stx-transfer? bounty-amount tx-sender (as-contract tx-sender)))
        (map-set job-postings
            { job-id: new-job-id }
            {
                company: tx-sender,
                title: title,
                bounty-amount: bounty-amount,
                status: "active",
                applications-count: u0
            }
        )
        (var-set job-nonce new-job-id)
        (ok new-job-id)
    )
)

;; #[allow(unchecked_data)]
(define-public (submit-application (job-id uint))
    (let
        (
            (job (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
            (new-application-id (+ (var-get application-nonce) u1))
        )
        (asserts! (is-eq (get status job) "active") err-invalid-status)
        (map-set applications
            { application-id: new-application-id }
            {
                job-id: job-id,
                applicant: tx-sender,
                status: "pending",
                interview-scheduled: false
            }
        )
        (map-set job-postings
            { job-id: job-id }
            (merge job { applications-count: (+ (get applications-count job) u1) })
        )
        (var-set application-nonce new-application-id)
        (ok new-application-id)
    )
)

;; #[allow(unchecked_data)]
(define-public (schedule-interview (application-id uint) (escrow-amount uint))
    (let
        (
            (application (unwrap! (map-get? applications { application-id: application-id }) err-not-found))
            (job (unwrap! (map-get? job-postings { job-id: (get job-id application) }) err-not-found))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status application) "pending") err-invalid-status)
        (try! (stx-transfer? escrow-amount tx-sender (as-contract tx-sender)))
        (map-set applications
            { application-id: application-id }
            (merge application { status: "interviewing", interview-scheduled: true })
        )
        (map-set escrows
            { application-id: application-id }
            { amount: escrow-amount, released: false }
        )
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (release-bounty (application-id uint))
    (let
        (
            (application (unwrap! (map-get? applications { application-id: application-id }) err-not-found))
            (job (unwrap! (map-get? job-postings { job-id: (get job-id application) }) err-not-found))
            (escrow (unwrap! (map-get? escrows { application-id: application-id }) err-not-found))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (asserts! (not (get released escrow)) err-invalid-status)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get applicant application))))
        (map-set escrows
            { application-id: application-id }
            (merge escrow { released: true })
        )
        (map-set applications
            { application-id: application-id }
            (merge application { status: "completed" })
        )
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (reject-application (application-id uint))
    (let
        (
            (application (unwrap! (map-get? applications { application-id: application-id }) err-not-found))
            (job (unwrap! (map-get? job-postings { job-id: (get job-id application) }) err-not-found))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status application) "pending") err-invalid-status)
        (map-set applications
            { application-id: application-id }
            (merge application { status: "rejected" })
        )
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (withdraw-application (application-id uint))
    (let
        (
            (application (unwrap! (map-get? applications { application-id: application-id }) err-not-found))
        )
        (asserts! (is-eq (get applicant application) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status application) "pending") err-invalid-status)
        (map-set applications
            { application-id: application-id }
            (merge application { status: "withdrawn" })
        )
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (cancel-job (job-id uint))
    (let
        (
            (job (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status job) "active") err-invalid-status)
        (asserts! (is-eq (get applications-count job) u0) (err u105))
        (try! (as-contract (stx-transfer? (get bounty-amount job) tx-sender (get company job))))
        (map-set job-postings
            { job-id: job-id }
            (merge job { status: "cancelled" })
        )
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (close-job (job-id uint))
    (let
        (
            (job (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (asserts! (is-eq (get status job) "active") err-invalid-status)
        (map-set job-postings
            { job-id: job-id }
            (merge job { status: "closed" })
        )
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (refund-escrow (application-id uint))
    (let
        (
            (application (unwrap! (map-get? applications { application-id: application-id }) err-not-found))
            (job (unwrap! (map-get? job-postings { job-id: (get job-id application) }) err-not-found))
            (escrow (unwrap! (map-get? escrows { application-id: application-id }) err-not-found))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (asserts! (not (get released escrow)) err-invalid-status)
        (asserts! (or (is-eq (get status application) "rejected") (is-eq (get status application) "withdrawn")) err-invalid-status)
        (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get company job))))
        (map-set escrows
            { application-id: application-id }
            (merge escrow { released: true })
        )
        (ok true)
    )
)

;; Rating system
(define-map company-ratings
    { company: principal }
    { total-score: uint, rating-count: uint }
)

(define-map applicant-ratings
    { applicant: principal }
    { total-score: uint, rating-count: uint }
)

(define-public (rate-company (job-id uint) (rating uint))
    (let
        (
            (job (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
            (current-rating (default-to { total-score: u0, rating-count: u0 } 
                (map-get? company-ratings { company: (get company job) })))
        )
        (asserts! (<= rating u5) (err u106))
        (asserts! (>= rating u1) (err u106))
        (map-set company-ratings
            { company: (get company job) }
            {
                total-score: (+ (get total-score current-rating) rating),
                rating-count: (+ (get rating-count current-rating) u1)
            }
        )
        (ok true)
    )
)

(define-public (rate-applicant (application-id uint) (rating uint))
    (let
        (
            (application (unwrap! (map-get? applications { application-id: application-id }) err-not-found))
            (job (unwrap! (map-get? job-postings { job-id: (get job-id application) }) err-not-found))
            (current-rating (default-to { total-score: u0, rating-count: u0 } 
                (map-get? applicant-ratings { applicant: (get applicant application) })))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (asserts! (<= rating u5) (err u106))
        (asserts! (>= rating u1) (err u106))
        (map-set applicant-ratings
            { applicant: (get applicant application) }
            {
                total-score: (+ (get total-score current-rating) rating),
                rating-count: (+ (get rating-count current-rating) u1)
            }
        )
        (ok true)
    )
)

(define-read-only (get-company-rating (company principal))
    (ok (map-get? company-ratings { company: company }))
)

(define-read-only (get-applicant-rating (applicant principal))
    (ok (map-get? applicant-ratings { applicant: applicant }))
)

;; Skills and categories
(define-map job-skills
    { job-id: uint, skill: (string-ascii 50) }
    { required: bool }
)

(define-map applicant-skills
    { applicant: principal, skill: (string-ascii 50) }
    { verified: bool, endorsements: uint }
)

;; #[allow(unchecked_data)]
(define-public (add-job-skill (job-id uint) (skill (string-ascii 50)))
    (let
        (
            (job (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (map-set job-skills
            { job-id: job-id, skill: skill }
            { required: true }
        )
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (add-applicant-skill (skill (string-ascii 50)))
    (begin
        (map-set applicant-skills
            { applicant: tx-sender, skill: skill }
            { verified: false, endorsements: u0 }
        )
        (ok true)
    )
)

;; #[allow(unchecked_data)]
(define-public (endorse-skill (applicant principal) (skill (string-ascii 50)))
    (let
        (
            (skill-data (unwrap! (map-get? applicant-skills { applicant: applicant, skill: skill }) err-not-found))
        )
        (map-set applicant-skills
            { applicant: applicant, skill: skill }
            (merge skill-data { endorsements: (+ (get endorsements skill-data) u1) })
        )
        (ok true)
    )
)

(define-read-only (get-job-skill (job-id uint) (skill (string-ascii 50)))
    (ok (map-get? job-skills { job-id: job-id, skill: skill }))
)

(define-read-only (get-applicant-skill (applicant principal) (skill (string-ascii 50)))
    (ok (map-get? applicant-skills { applicant: applicant, skill: skill }))
)

;; Platform fee management
(define-data-var platform-fee-percentage uint u5)
(define-data-var total-fees-collected uint u0)

(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u20) (err u107))
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)

(define-read-only (get-platform-fee)
    (ok (var-get platform-fee-percentage))
)

(define-read-only (get-total-fees-collected)
    (ok (var-get total-fees-collected))
)

(define-public (collect-platform-fee (application-id uint))
    (let
        (
            (application (unwrap! (map-get? applications { application-id: application-id }) err-not-found))
            (job (unwrap! (map-get? job-postings { job-id: (get job-id application) }) err-not-found))
            (fee-amount (/ (* (get bounty-amount job) (var-get platform-fee-percentage)) u100))
        )
        (asserts! (is-eq (get status application) "completed") err-invalid-status)
        (try! (stx-transfer? fee-amount (get company job) contract-owner))
        (var-set total-fees-collected (+ (var-get total-fees-collected) fee-amount))
        (ok fee-amount)
    )
)

;; Job categories
(define-map job-categories
    { job-id: uint }
    { category: (string-ascii 50) }
)

;; #[allow(unchecked_data)]
(define-public (set-job-category (job-id uint) (category (string-ascii 50)))
    (let
        (
            (job (unwrap! (map-get? job-postings { job-id: job-id }) err-not-found))
        )
        (asserts! (is-eq (get company job) tx-sender) err-unauthorized)
        (map-set job-categories
            { job-id: job-id }
            { category: category }
        )
        (ok true)
    )
)

(define-read-only (get-job-category (job-id uint))
    (ok (map-get? job-categories { job-id: job-id }))
)

;; Application notes
(define-map application-notes
    { application-id: uint }
    { note: (string-ascii 500) }
)

;; #[allow(unchecked_data)]
(define-public (add-application-note (application-id uint) (note (string-ascii 500)))
    (let
        (
            (application (unwrap! (map-get? applications { application-id: application-id }) err-not-found))
        )
        (asserts! (is-eq (get applicant application) tx-sender) err-unauthorized)
        (map-set application-notes
            { application-id: application-id }
            { note: note }
        )
        (ok true)
    )
)

(define-read-only (get-application-note (application-id uint))
    (ok (map-get? application-notes { application-id: application-id }))
)

;; Statistics tracking
(define-data-var total-jobs-posted uint u0)
(define-data-var total-applications-submitted uint u0)
(define-data-var total-interviews-scheduled uint u0)
(define-data-var total-hires-completed uint u0)

(define-public (update-job-statistics)
    (begin
        (var-set total-jobs-posted (+ (var-get total-jobs-posted) u1))
        (ok true)
    )
)

(define-public (update-application-statistics)
    (begin
        (var-set total-applications-submitted (+ (var-get total-applications-submitted) u1))
        (ok true)
    )
)

(define-read-only (get-platform-statistics)
    (ok {
        total-jobs: (var-get total-jobs-posted),
        total-applications: (var-get total-applications-submitted),
        total-interviews: (var-get total-interviews-scheduled),
        total-hires: (var-get total-hires-completed)
    })
)