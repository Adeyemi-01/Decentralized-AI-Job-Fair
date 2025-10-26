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