;; ===============================================
;; BASIX Music-IP Protocol - Optimized Logic Layer
;; ===============================================
;; Maintains pure, deterministic logic. All I/O, state changes, and
;; blockchain interactions are delegated to Python via grounding functions.

;; ---------- Type Definitions & Core Vocabulary ----------
;; Declare types for clarity and future validation
(type Role)
(type ModuleType)
(type AssetType)
(type Genre)
(type Address String) ; Ethereum-style address
(type BPS Number) ; Basis Points (0-10000)
(type WorkID String)
(type ModuleID String)
(type ProofID String)

(role ARTIST) (role PRODUCER) (role LYRICIST) (role PERFORMER) (role REMIXER) (role LABEL)
(module-type MASTER) (module-type LYRICS) (module-type PERFORMANCE) (module-type REMIX) (module-type STEM)
(asset-type TICKET) (asset-type MERCH) (asset-type EXPERIENCE)
(genre HIPHOP) (genre AFROBEATS) (genre SOLO) ; ... etc.

;; ---------- Result Type for Robust Error Handling ----------
;; Standardized output for Python runtime. Either contains a value or an error string.
(type Result (Ok Any) (Err String))

(= (ok ?value) (Ok !value)) ; ! asserts the value into the AtomSpace for the result
(= (error ?msg) (Err !msg))

;; ---------- Optimized Split Template Management ----------
;; Pre-computed templates as lists for fast access
(= (split-template HIPHOP)   (list (pair PRODUCER 4000) (pair ARTIST 3500) (pair LYRICIST 2000) (pair LABEL 500)))
(= (split-template AFROBEATS)(list (pair PRODUCER 3500) (pair ARTIST 4000) (pair LYRICIST 1500) (pair LABEL 1000)))
(= (split-template SOLO)     (list (pair ARTIST 8000) (pair LYRICIST 2000)))

;; Resolve a role for a given work to a payment address.
;; This is a primary grounding point. Python provides the mapping.
(= (resolve-role-address ?workId ?role)
   (let (($result (grounded-call "resolve_role" (list ?workId ?role)))) ; Returns (Ok Address) | (Err String)
     (match $result
        ((Ok ?addr) ?addr)
        ((Err ?msg) (error ?msg)))))

;; ---------- Core Splits Computation (Memoized Pattern) ----------
;; Avoids recomputation by relying on the AtomSpace as a cache.
;; If explicit splits exist, use them. Else, use genre template.
(= (compute-splits ?workId)
   (match &self
      ;; Case 1: Explicit splits provided - highest priority
      (explicit-splits ?workId ?splitsList) ?splitsList

      ;; Case 2: Use genre-based template
      (genre ?workId ?genreTag)
      (let (($template (split-template ?genreTag)))
         (map (lambda ($roleBpsPair)
                (let (($role (fst $roleBpsPair))
                      ($bps (snd $roleBpsPair))
                      ($addr (resolve-role-address ?workId $role)))
                  (if (is-error $addr) ; Propagate error if resolve failed
                      $addr
                      (pair $addr $bps))))
              $template))

      ;; Case 3: Fallback
      (compute-splits ?workId (split-template SOLO))))

;; ---------- Validation as a Separate, Pure Function ----------
(= (validate-splits ?splitsList)
   (let (($sum (fold + 0 (map snd ?splitsList))))
     (if (not (= $sum 10000))
         (error "BPS sum is not 10000")
         (let (($addrs (map fst ?splitsList)))
           (if (= (length $addrs) (length (unique $addrs)))
               (ok ?splitsList)
               (error "Duplicate addresses in splits"))))))

;; ---------- Provenance & Derivative Logic ----------
;; Relies heavily on grounding for expensive comparisons (audio analysis)
(= (is-derivative-of ?childWorkId ?parentWorkId)
   (or (declared-derivative ?childWorkId ?parentWorkId)
       (sampled ?childWorkId ?parentWorkId)
       (let (($similarity (grounded-call "audio_similarity" (list ?childWorkId ?parentWorkId))))
         (>= $similarity 0.30)) ; Configurable threshold
       (grounded-call "watermark_detected" (list ?childWorkId ?parentWorkId))))

;; Recursively find the direct parent of a derivative work
(= (get-direct-parent ?workId)
   (match &self
      (is-derivative-of ?workId ?parentId) ?parentId
      _ nil))

;; ---------- Dynamic Pricing with Sanitized Inputs ----------
;; All external data (streams, social) is fetched via one grounding call
;; for efficiency and atomicity.
(= (fetch-market-data ?workId)
   (grounded-call "get_market_data" (list ?workId))) ; Returns dict of {streams, social, liquidity}

(= (compute-price ?workId $basePrice $weights $bands)
   (let (($data (fetch-market-data ?workId)) ; $data is a dictionary
         ($streamsFactor (get $data "streams" 0.5))
         ($socialFactor (get $data "social" 0.5))
         ($liquidityFactor (get $data "liquidity" 0.5)))
     (let (($rawPrice (* $basePrice
                         (+ 1.0
                            (+ (* (get $weights "w_streams") $streamsFactor)
                               (* (get $weights "w_social") $socialFactor)
                               (* (get $weights "w_liquidity") $liquidityFactor))))))
       (max (min $rawPrice (get $bands "max")) (get $bands "min")))) ; Clamp to band
)

;; ---------- Final Output Functions for Python Runtime ----------
;; These functions are the primary API. They return a standardized `Result`
;; type containing either the desired data structure or an error message.

(= (get-registration-payload ?workId ?workHash ?controllerAddr)
   (match (validate-splits (compute-splits ?workId))
      ((Ok ?validSplits) (ok (dict
                             ("workId" ?workId)
                             ("workHash" ?workHash)
                             ("controller" ?controllerAddr)
                             ("splits" ?validSplits)
                             ("modules" (grounded-call "get_work_modules" (list ?workId))))))
      ((Err ?msg) (error ?msg))))

(= (get-royalty-distribution ?workId ?totalRevenue)
   (match (validate-splits (compute-splits ?workId))
      ((Ok ?validSplits)
         (ok (map (lambda ($split)
                    (dict
                     ("to" (fst $split))
                     ("amount" (/ (* (snd $split) ?totalRevenue) 10000))))
                  $validSplits)))
      ((Err ?msg) (error ?msg))))

(= (check-derivative-status ?newWorkId)
   (let (($parentId (get-direct-parent ?newWorkId)))
     (if $parentId
         (ok (dict
              ("isDerivative" True)
              ("parentWorkId" $parentId)
              ("recommendedAction" "RECALCULATE_SPLITS")))
         (ok (dict ("isDerivative" False))))))

;; ---------- Insurance Check (Idempotent) ----------
(= (evaluate-insurance-trigger ?workId)
   (let (($streams (get (fetch-market-data ?workId) "streams" 0))
         ($days (grounded-call "days_since_release" (list ?workId)))
         ($config (dict ("horizon" 120) ("threshold" 1000000))))
     (if (and (>= $days (get $config "horizon"))
              (< $streams (get $config "threshold")))
         (ok (dict ("triggered" True) ("reimburseBps" 2000)))
         (ok (dict ("triggered" False))))))