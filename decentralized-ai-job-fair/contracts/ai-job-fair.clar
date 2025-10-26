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