;; ScholarChain: Decentralized Academic Credential Verification System
;; Version: 1.0.0
;; A protocol for secure academic credential issuance, verification, and employer validation

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u1))
(define-constant ERR-CREDENTIAL-NOT-FOUND (err u2))
(define-constant ERR-INVALID-TUITION (err u3))
(define-constant ERR-INVALID-DURATION (err u4))
(define-constant ERR-INVALID-PROGRAM (err u5))
(define-constant ERR-INVALID-DESCRIPTION (err u6))
(define-constant ERR-CREDENTIAL-INACTIVE (err u7))
(define-constant ERR-ALREADY-ENROLLED (err u8))
(define-constant ERR-NOT-ENROLLED (err u9))
(define-constant ERR-INSUFFICIENT-FUNDS (err u10))
(define-constant ERR-GRADUATION-NOT-READY (err u11))
(define-constant ERR-ALREADY-GRADUATED (err u12))
(define-constant ERR-INVALID-LEVEL (err u13))
(define-constant ERR-INVALID-TYPE (err u14))
(define-constant ERR-PROGRAM-ACTIVE (err u15))
(define-constant ERR-INVALID-AMOUNT (err u16))

;; Constants
(define-constant MIN-TUITION u500000) ;; 0.5 STX minimum
(define-constant MAX-TUITION u500000000000) ;; 500K STX maximum
(define-constant MIN-DURATION u2592000) ;; 30 days minimum
(define-constant MAX-DURATION u126144000) ;; 4 years maximum
(define-constant VERIFICATION-FEE-PERCENT u3) ;; 3% verification fee
(define-constant GRADUATION-THRESHOLD u85) ;; 85% minimum score for graduation

;; Data variables
(define-data-var next-credential-id uint u1)
(define-data-var next-enrollment-id uint u1)
(define-data-var accreditation-board principal tx-sender)
(define-data-var total-verification-fees uint u0)

;; Credential data structure
(define-map credentials
    uint
    {
        institution: principal,
        program-name: (string-utf8 100),
        description: (string-utf8 500),
        academic-level: (string-utf8 20),
        program-type: (string-utf8 10),
        tuition-amount: uint,
        bond-amount: uint,
        program-duration: uint,
        is-active: bool,
        total-enrollments: uint,
        total-graduates: uint,
        created-at: uint
    })

;; Enrollment data structure
(define-map enrollments
    uint
    {
        student: principal,
        credential-id: uint,
        enrolled-at: uint,
        graduation-at: uint,
        academic-score: uint,
        is-graduated: bool,
        is-verified: bool,
        bond-locked: uint
    })

;; Student enrollments by credential
(define-map student-credential-enrollments
    { student: principal, credential-id: uint }
    uint)

;; Graduation records
(define-map graduations
    { student: principal, credential-id: uint }
    {
        graduated-at: uint,
        final-score: uint,
        certificate-hash: (string-utf8 64)
    })

;; Private validation functions
(define-private (validate-academic-level (academic-level (string-utf8 20)))
    (or 
        (is-eq academic-level u"Certificate")
        (is-eq academic-level u"Diploma")
        (is-eq academic-level u"Associate")
        (is-eq academic-level u"Bachelor")
        (is-eq academic-level u"Master")
        (is-eq academic-level u"Doctorate")
        (is-eq academic-level u"Professional")
        (is-eq academic-level u"Postdoc")
    ))

(define-private (validate-program-type (program-type (string-utf8 10)))
    (or 
        (is-eq program-type u"Online")
        (is-eq program-type u"Campus")
        (is-eq program-type u"Hybrid")
        (is-eq program-type u"Research")
    ))

(define-private (validate-text-length (text (string-utf8 500)) (min-length uint) (max-length uint))
    (let 
        (
            (text-length (len text))
        )
        (and 
            (>= text-length min-length)
            (<= text-length max-length)
        )
    ))

(define-private (calculate-verification-fee (amount uint))
    (/ (* amount VERIFICATION-FEE-PERCENT) u100))

(define-private (calculate-institution-amount (amount uint))
    (- amount (calculate-verification-fee amount)))

(define-private (validate-bond-amount (bond-amount uint))
    (and (>= bond-amount u0) (<= bond-amount u50000000000))) ;; Max 50K STX bond

(define-private (validate-certificate-hash (certificate-hash (string-utf8 64)))
    (and (>= (len certificate-hash) u32) (<= (len certificate-hash) u64)))

;; Public functions

;; Create a new academic credential program
(define-public (create-credential 
    (program-name (string-utf8 100))
    (description (string-utf8 500))
    (academic-level (string-utf8 20))
    (program-type (string-utf8 10))
    (tuition-amount uint)
    (bond-amount uint)
    (program-duration uint))
    (let
        (
            (credential-id (var-get next-credential-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate inputs
        (asserts! (validate-text-length program-name u3 u100) ERR-INVALID-PROGRAM)
        (asserts! (validate-text-length description u10 u500) ERR-INVALID-DESCRIPTION)
        (asserts! (validate-academic-level academic-level) ERR-INVALID-LEVEL)
        (asserts! (validate-program-type program-type) ERR-INVALID-TYPE)
        (asserts! (and (>= tuition-amount MIN-TUITION) (<= tuition-amount MAX-TUITION)) ERR-INVALID-TUITION)
        (asserts! (and (>= program-duration MIN-DURATION) (<= program-duration MAX-DURATION)) ERR-INVALID-DURATION)
        (asserts! (validate-bond-amount bond-amount) ERR-INVALID-TUITION)
        
        ;; Create credential
        (map-set credentials credential-id {
            institution: tx-sender,
            program-name: program-name,
            description: description,
            academic-level: academic-level,
            program-type: program-type,
            tuition-amount: tuition-amount,
            bond-amount: bond-amount,
            program-duration: program-duration,
            is-active: true,
            total-enrollments: u0,
            total-graduates: u0,
            created-at: current-time
        })
        
        (var-set next-credential-id (+ credential-id u1))
        (ok credential-id)
    ))

;; Enroll in academic program with bond
(define-public (enroll-in-program (credential-id uint))
    (let
        (
            (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
            (enrollment-id (var-get next-enrollment-id))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (graduation-at (+ current-time (get program-duration credential)))
            (total-cost (+ (get tuition-amount credential) (get bond-amount credential)))
            (verification-fee (calculate-verification-fee (get tuition-amount credential)))
            (institution-amount (calculate-institution-amount (get tuition-amount credential)))
        )
        ;; Validate credential is active
        (asserts! (get is-active credential) ERR-CREDENTIAL-INACTIVE)
        
        ;; Check if already enrolled
        (asserts! (is-none (map-get? student-credential-enrollments { student: tx-sender, credential-id: credential-id })) ERR-ALREADY-ENROLLED)
        
        ;; Transfer tuition to institution and verification fee
        (try! (stx-transfer? institution-amount tx-sender (get institution credential)))
        (try! (stx-transfer? verification-fee tx-sender (var-get accreditation-board)))
        
        ;; Lock bond amount (simulated by requiring balance)
        (asserts! (>= (stx-get-balance tx-sender) (get bond-amount credential)) ERR-INSUFFICIENT-FUNDS)
        
        ;; Create enrollment
        (map-set enrollments enrollment-id {
            student: tx-sender,
            credential-id: credential-id,
            enrolled-at: current-time,
            graduation-at: graduation-at,
            academic-score: u0,
            is-graduated: false,
            is-verified: false,
            bond-locked: (get bond-amount credential)
        })
        
        ;; Map student to enrollment
        (map-set student-credential-enrollments { student: tx-sender, credential-id: credential-id } enrollment-id)
        
        ;; Update credential stats
        (map-set credentials credential-id (merge credential { total-enrollments: (+ (get total-enrollments credential) u1) }))
        
        ;; Update verification fees
        (var-set total-verification-fees (+ (var-get total-verification-fees) verification-fee))
        (var-set next-enrollment-id (+ enrollment-id u1))
        
        (ok enrollment-id)
    ))

;; Update academic progress
(define-public (update-academic-score (credential-id uint) (academic-score uint))
    (let
        (
            (enrollment-id (unwrap! (map-get? student-credential-enrollments { student: tx-sender, credential-id: credential-id }) ERR-NOT-ENROLLED))
            (enrollment (unwrap! (map-get? enrollments enrollment-id) ERR-NOT-ENROLLED))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
        )
        ;; Validate enrollment is active
        (asserts! (< current-time (get graduation-at enrollment)) ERR-PROGRAM-ACTIVE)
        (asserts! (<= academic-score u100) ERR-INVALID-AMOUNT)
        (asserts! (>= academic-score (get academic-score enrollment)) ERR-INVALID-AMOUNT)
        
        ;; Update academic score
        (map-set enrollments enrollment-id (merge enrollment { 
            academic-score: academic-score,
            is-verified: (>= academic-score u100)
        }))
        
        (ok true)
    ))

;; Process graduation
(define-public (process-graduation (credential-id uint) (certificate-hash (string-utf8 64)))
    (let
        (
            (enrollment-id (unwrap! (map-get? student-credential-enrollments { student: tx-sender, credential-id: credential-id }) ERR-NOT-ENROLLED))
            (enrollment (unwrap! (map-get? enrollments enrollment-id) ERR-NOT-ENROLLED))
            (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
            (current-time (unwrap-panic (get-stacks-block-info? time (- stacks-block-height u1))))
            (validated-credential-id (get credential-id enrollment))
            (validated-hash certificate-hash)
        )
        ;; Additional validations
        (asserts! (validate-certificate-hash certificate-hash) ERR-INVALID-DESCRIPTION)
        (asserts! (is-eq credential-id validated-credential-id) ERR-CREDENTIAL-NOT-FOUND)
        
        ;; Validate score and graduation readiness
        (asserts! (get is-verified enrollment) ERR-GRADUATION-NOT-READY)
        (asserts! (>= (get academic-score enrollment) GRADUATION-THRESHOLD) ERR-GRADUATION-NOT-READY)
        (asserts! (not (get is-graduated enrollment)) ERR-ALREADY-GRADUATED)
        
        ;; Process graduation record
        (map-set graduations { student: tx-sender, credential-id: validated-credential-id } {
            graduated-at: current-time,
            final-score: (get academic-score enrollment),
            certificate-hash: validated-hash
        })
        
        ;; Update enrollment
        (map-set enrollments enrollment-id (merge enrollment { is-graduated: true }))
        
        ;; Update credential stats
        (map-set credentials validated-credential-id (merge credential { total-graduates: (+ (get total-graduates credential) u1) }))
        
        ;; Return bond to student (simulated)
        (ok true)
    ))

;; Deactivate credential program (institution only)
(define-public (deactivate-credential (credential-id uint))
    (let
        (
            (credential (unwrap! (map-get? credentials credential-id) ERR-CREDENTIAL-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender (get institution credential)) ERR-NOT-AUTHORIZED)
        (map-set credentials credential-id (merge credential { is-active: false }))
        (ok true)
    ))

;; Read-only functions
(define-read-only (get-credential (credential-id uint))
    (map-get? credentials credential-id))

(define-read-only (get-enrollment (enrollment-id uint))
    (map-get? enrollments enrollment-id))

(define-read-only (get-student-enrollment (student principal) (credential-id uint))
    (match (map-get? student-credential-enrollments { student: student, credential-id: credential-id })
        enrollment-id (map-get? enrollments enrollment-id)
        none
    ))

(define-read-only (get-graduation (student principal) (credential-id uint))
    (map-get? graduations { student: student, credential-id: credential-id }))

(define-read-only (is-student-graduated (student principal) (credential-id uint))
    (is-some (map-get? graduations { student: student, credential-id: credential-id })))

(define-read-only (get-credential-stats (credential-id uint))
    (match (map-get? credentials credential-id)
        credential {
            total-enrollments: (get total-enrollments credential),
            total-graduates: (get total-graduates credential),
            graduation-rate: (if (> (get total-enrollments credential) u0)
                (/ (* (get total-graduates credential) u100) (get total-enrollments credential))
                u0
            )
        }
        { total-enrollments: u0, total-graduates: u0, graduation-rate: u0 }
    ))

(define-read-only (get-accreditation-stats)
    {
        total-credentials: (- (var-get next-credential-id) u1),
        total-enrollments: (- (var-get next-enrollment-id) u1),
        total-verification-fees: (var-get total-verification-fees),
        accreditation-board: (var-get accreditation-board)
    })

(define-read-only (calculate-program-cost (credential-id uint))
    (match (map-get? credentials credential-id)
        credential {
            tuition: (get tuition-amount credential),
            bond: (get bond-amount credential),
            total: (+ (get tuition-amount credential) (get bond-amount credential)),
            verification-fee: (calculate-verification-fee (get tuition-amount credential)),
            institution-amount: (calculate-institution-amount (get tuition-amount credential))
        }
        { tuition: u0, bond: u0, total: u0, verification-fee: u0, institution-amount: u0 }
    ))