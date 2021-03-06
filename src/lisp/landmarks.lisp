
(in-package :mizar)

(defparameter *registered-ids* nil
  "IDs of landmarks that have been registered so far with us.  The purpose of this list is to ensure that we never create two distinct landmarks with the same ID.")

(defclass landmark ()
  ((pretty-name
    :initform nil
    :initarg :pretty-name
    :accessors pretty-name
    :documentation "Human-readable name for a landmark."
    :type (or nil string))
   (escaped-html-name
    :initform nil
    :initarg :html-name
    :accessor escaped-html-name
    :documentation "An presentation of the landmarks's name suitable for HTML contexts in which elements aren't allowed, as when a name is used as an attribute."
    :type (or nil string))
   (id
    :initform nil
    :initarg :id
    :accessor id
    :documentation "A unique identifier for this landmark.  It should be suitable for use as the value of an HTML ID attribute"
    :type (or nil string))
   (article
    :initform nil
    :initarg :article
    :accessor article
    :documentation "The article that represents or contains this landmark."
    :type (or nil string))
   (item-kind
    :initform nil
    :initarg :item-kind
    :accessor item-kind
    :documentation "The item kind (theorem, lemma, etc.) of this landmark, if applicable."
    :type (or nil string))
   (item-number
    :initform nil
    :initarg :item-number
    :accessor item-number
    :documentation "The item number of this landmark, relative to ARTICLE and ITEM-KIND, if applicable."
    :type (or nil fixnum))
   (parent
    :initform nil
    :initarg :parent
    :accessor parent
    :documentation "The 'parent' of this landmark, if this landmark is a variant of some other landmark."
    :type (or nil landmark))
   (variants
    :initform nil
    :initarg :variants
    :accessor variants
    :documentation "A list of (possibly anonymous) landmarks that are variants of this one."
    :type list)))

(defun make-landmark (&key pretty-name escaped-html-name
		           article item-kind item-number id variants)
  (if (member id *registered-ids* :test #'string=)
      (error "Cannot make a new landmark with the ID '~a', because there is always a landmark with this ID.")
      (make-instance 'landmark
		     :pretty-name pretty-name
		     :escaped-html-name escaped-html-name
		     :article article
		     :item-kind item-kind
		     :item-number item-number
		     :id id
		     :variants variants)))

(defparameter *landmarks* nil
  "A list of registered landmarks.")

(defmacro register-landmark (&key pretty-name escaped-html-name
			          article item-kind item-number id variants)
  `(push (make-landmark :pretty-name ,pretty-name
			:id ,id
			:article ,article
			:item-kind ,item-kind
			:item-number ,item-number) *landmarks*))

(register-landmark :pretty-name "Armstrong's Axioms"
		   :id "armstrongs-axioms"
		   :article "armstrng")

(let* ((arrow (make-landmark :pretty-name "Arrow's Impossibility Theorem"
			     :id "arrows-impossibility-theorem"))
       (arrow-weak (make-landmark :pretty-name "For weak orders"
				  :id "arrows-impossibility-theorem-for-weak-orders"
				  :article "arrow"
				  :item-kind "theorem"
				  :item-number 14
				  :parent arrow))
       (arrow-strong (make-landmark :pretty-name "A stronger version"
				    :id "arrows-impossibility-theorem-a-stronger-version"
				    :article "arrow"
				    :item-kind "theorem"
				    :item-number 15
				    :parent arrow)))
  (setf (variants arrow) (list arrow-weak arrow-strong))
  (push arrow *landmarks*))

;;     ("Alexander's Lemma" "/item/waybel_7/theorem/35")
;;     ("Armstrong's Axioms" "/article/armstrng")
;;     ("Arrow's Impossibility Theorem" 
;;      (("For weak orders" "/item/arrow/theorem/14")
;;       ("Stronger version" "/item/arrow/theorem/15")))
;;     ("Axiom of Choice" "/item/wellord2/theorem/27")
;;     ("Axiom of Regularity" "/item/tarski/theorem/7")
;;     ("Baire Category Theorem"
;;      (("For Continuous Lattices" "/item/waybel12/theorem/43")
;;       ("For Banach Spaces" "/item/lopban_5/theorem/3")
;;       ("For Hausdorff Spaces" "/item/normsp_2/theorem/10")))
;;     ("Banach Fix Point Theorem for Compact Spaces" "/item/ali2/theorem/2")
;;     ("Banach-Steinhaus theorem (Uniform Boundedness)"
;;      ("/item/lopban_5/theorem/7" "/item/lopban_5/theorem/8"))
;;     ("Bertrand's Postulate" "/item/nat_4/theorem/56")
;;     ("Bing's Theorem" "/item/nagata_2/theorem/22")
;;     ("Binomial Theorem" "/item/binom/theorem/26")
;;     ("Birkhoff Variety Theorem" "/item/birkhoff/scheme/12")
;;     ("Bolzano Intermediate Value Theorem " "/item/topreal5/theorem/14")
;;     ("Bolzano-Weierstrass (1-dimension)" "/item/seq_4/theorem/57")
;;     ("Borsuk Theorem on Decomposition of Strong Deformation Retracts" "/item/borsuk_1/theorem/85")
;;     ("Brouwer Fixed Point Theorem"
;;      (("For Disks on the Plane" "/item/brouwer/theorem/15")
;;       ("For Intervals" "/item/treal_1/theorem/27")))
;;     ("Brown Theorem" "/item/gcd_1/theorem/41")
;;     ("Cantor Theorem" "/item/card_1/theorem/30")
;;     ("(Almost) Cantor-Bernstein Theorem" "/item/card_1/theorem/26")
;;     ("Caratheodory's Theorem" "/item/measure4/theorem/23")
;;     ("Carmichael's Theorem on Prime Divisors" "/item/fib_num2/theorem/71")
;;     ("Cauchy Sequence" "/item/seq_4/theorem/58")
;;     ("Cauchy Theorem" "/item/rolle/theorem/6")
;;     ("Chinese Remainder Theorem" "/item/wsierp_1/theorem/43")
;;     ("Cographs (Series-Parallel)" "/article/nackla_3")
;;     ("Compactness of lim-inf Topology" "/item/waybel33/theorem/27")
;;     ("Contraction Lemma" "/item/zf_colla/theorem/14")
;;     ("Convergents Of Continued fraction" "/fragment/real_3/97")
;;     ("Euclid's Greatest Common Divisor Algorithm" "/item/nat_d/scheme/1")
;;     ("Cyclotomic Polynomials" "/article/uniroots")
;;     ("Darboux's Theorem" "/item/integra3/theorem/19")
;;     ("Deduction Theorem" "/item/cqc_the2/theorem/96")
;;     ("Desargues Theorem" "/item/projdes1/theorem/27")
;;     ("Dickson Lemma" "/item/dickson/fcluster/17") ; probably not accurate
;;     ("Dijkstra's Shortest-Path Algorithm"
;;      ("/item/glib_004/theorem/27" "/item/graph_5/theorem/69"))
;;     ("Dilworth's Decomposiiton Theorem"
;;      (("Finite case" "/item/dilworth/theorem/51")
;;       ("Maxsc" "/item/dilworth/theorem/53")
;;       ("General case" "/item/dilworth/theorem/54")))
;;     ("Dirichlet Principle/Pigeon Hole Principle" "/item/finseq_4/theorem/80")
;;     ("Dynkin Lemma" "/item/dynkin/theorem/27")
;;     ("Egorov's Theorem" "/item/mesfunc8/theorem/30")
;;     ("Erd&#337;s-Szekeres Theorem"
;;      ("/item/dilworth/theorem/55" "/item/dilworth/theorem/56"))
;;     ("Euler's Theorem" "/item/euler_2/theorem/33")
;;     ("Existence of Cantor Normal Form for Ordinal Numbers" "/item/ordinal5/theorem/69")
;;     ("Extensionality Axiom" "/item/tarski/theorem/2")
;;     ("A Corollary from the Poincare-Miranda Theorem (proven from scratch and named Fashoda...)" "/item/jgraph_1/theorem/65")
;;     ("Fatou's Lemma" "/item/mesfun10/theorem/7")
;;     ("Fermat's Little Theorem" "/item/euler_2/theorem/34")
;;     ("Ford/Fulkerson Maximum Flow Algorithm" "/item/glib_005/theorem/19")
;;     ("Fraenkel/Replacement Scheme" "/item/tarski/scheme/1")
;;     ("Frattini Subgroup" "/article/group_4")
;;     ("Fundamental Theorem of Algebra" "/item/polynom5/theorem/75")
;;     ("Fundamental Theorem of Arithmetic"
;;      (("Every natural number is the product of its prime-power decomposition" "/item/nat_3/theorem/61")
;;       ("Uniqueness of prime-power decomposition" "/item/int_7/theorem/16")))
;;     ("G&ouml;del Completeness Theorem" "/item/goedelcp/theorem/38")
;;     ("Group isomorphism theorem"
;;      (("First" "/item/group_6/theorem/91")
;;       ("Second" "/item/group_6/theorem/93")
;;       ("Third" "/item/group_6/theorem/92")))
;;     ("Hahn-Banach Theorem"
;;      (("For complex spaces" "/item/hahnban1/theorem/36")
;;       ("For real spaces" "/item/hahnban/theorem/37")))
;;     ("Hall Marriage Theorem" "/item/hallmar1/theorem/32")
;;     ("Heine-Borel Theorem (for intervals)" "/item/heine/theorem/11")
;;     ("Heron's Formula" "/item/euclid_6/theorem/39")
;;     ("Hessenberg Theorem" 
;;      (("Direct proof" "/item/card_4/theorem/77")
;;       ("From Desargues' Theorem" "/item/hessenbe/rcluster/1"))) ; probably not accurate
;;     ("Hilbert Basis Theorem" "/item/hilbasis/fcluster/6") ; probably not accurate
;;     ("Integral of Measurable Function" "/item/mesfunc5/deftheorem/16") ; probably not accurate
;;     ("IntermedIate Value Theorem" "/article/topreal5")
;;     ("Irrationality of e" "/item/irrat_1/theorem/42") ; "e" should be italicized
;;     ("Isosceles Triangle Theorem"
;;      ("/item/euclid_6/theorem/18" "/item/euclid_6/theorem/19"))
;;     ("Intersecting Chords Theorem" "/item/euclid_6/theorem/38")
;;     ("Jonsson Theorem"
;;      (("For Lattices" "/item/lattice5/theorem/47")
;;       ("For Modular Lattices" "/item/lattice8/theorem/37")))
;;     ("Jordan Curve Theorem"
;;      (("General case" "/item/jordan/theorem/98")
;;       ("For special polygons" "/item/gobrd12/theorem/11")))
;;     ("Jordan Matrix Decomposition Theorem" "/item/matrixj2/theorem/32")
;;     ("Jordan-H&ouml;lder Theorem" "/item/group_9/theorem/119")
;;     ("K&ouml;nig Lemma" "/item/trees_2/theorem/32")
;;     ("Koenig Theorem" "/item/card_3/theorem/56")
;;     ("Kolgomorov's Zero-One Law" "/item/kolmog01/theorem/16")
;;     ("Kronecker-Capelli theorem" "/item/matrix15/theorem/57")
;;     ("Kuratowski-Zorn Lemma" "/item/orders_1/theorem/175")
;;     ("Kuratowski Closure-Complement Problem" "/item/kurato_1/theorem/67")
;;     ("Kuratowski Convrgence" "/article/kurato_2")
;;     ("Lagrange Theorem" "/item/rolle/theorem/4")
;;     ("Lagrange Theorem for Groups" "/item/group_2/theorem/177")
;;     ("Laplace Expansion" "/article/laplace")
;;     ("Law of Cosines" "/item/euclid_6/theorem/7")
;;     ("Lebesgue's Bounded Convergence Theorem" "/item/mesfun10/theorem/20")
;;     ("Lebesgue's Monotone Convergence Theorem" "/item/mesfunc9/theorem/52")
;;     ("Lebesgue's Covering Lemma" "/item/uniform1/theorem/7")
;;     ("Lebesgue Measure (dimension 1)" "/article/measure7")
;;     ("Lexicographic Breadth-first Search" "/item/lexbfs/theorem/50")
;;     ("Lipschitz Continuity" "/item/fcont_1/rcluster/5") ; probably not accurate
;;     ("Little Bezout Theorem (Factor Theorem)" "/item/uproots/theorem/52")
;;     ("Lucas Numbers" "/item/fib_num3/deftheorem/1")
;;     ("Mean Value Theorem for Integrals (first)" "/item/mesfunc7/theorem/17")
;;     ("Meister-Gauss Formula (for triangles)" "/item/euclid_6/deftheorem/1")
;;     ("Minkowski Inequality"
;;      ("/item/hermitan/theorem/50" "/item/hermitan/theorem/51"))
;;     ("M&ouml;bius Function" "/article/moebius1")
;;     ("Multiplication of Polynomials using Discrete Fourier Transformation" "/item/polynom8/theorem/44")
;;     ("Myhill-Nerode theorem" "/item/fsm_1/theorem/85")
;;     ("Nagata-Smirnov Theorem" "/item/nagata_2/theorem/19")
;;     ("Niemytzki Plane" "/article/topgen_5")
;;     ("Open Mapping Theorem" "/item/lopban_6/theorem/16")
;;     ("Pepin's Test" "/item/pepin/theorem/63")
;;     ("Pocklington's Theorem" "/item/nat_4/theorem/25")
;;     ("Prim's Minimum Spanning Tree Algorithm" "/item/glib_004/theorem/43")
;;     ("Ptolemy Theorem" "/item/euclid_6/theorem/40")
;;     ("Pythagorean Triples" "/article/pythtrip")
;;     ("Quotient Ring" "/item/ring_1/deftheorem/6")
;;     ("Ramsey Theorem" "/item/ramsey_1/theorem/17")
;;     ("Rank-nullity Theorem"
;;      ("/item/ranknull/theorem/44" "/item/ranknull/theorem/45"))
;;     ("Reflection Theorem" "/item/zf_refle/theorem/29")
;;     ("Representation Theorem for Finite Distributive Lattices" "/item/lattice7/theorem/16")
;;     ("Rolle Theorem" "/item/rolle/theorem/2")
;;     ("Schreier Refinement Theorem" "/item/group_9/theorem/118")
;;     ("Schroeder Bernstein Theorem" "/item/knaster/theorem/12")
;;     ("Schur's Criterion" "/item/hurwitz/theorem/54")
;;     ("Cauchy-Schwarz Inequality"
;;      ("/item/hermitan/theorem/48" "/item/hermitan/theorem/49"))
;;     ("Sorgenfrey Line" "/article/topgen_3")
;;     ("Stirling Numbers of the Second Kind" "/article/stirl2_1")
;;     ("Sylow's Theorem"
;;      (("First" "/item/group_10/theorem/12")
;;       ("Second" "/item/group_10/theorem/14")
;;       ("Third" "/item/group_10/theorem/15")))
;;     ("Steinitz Theorem"
;;      ("/item/vectsp_9/theorem/23" "/item/rlvect_5/theorem/23"))
;;     ("Stone Representation Theorem"
;;      (("For Boolean Algebras" "/item/lopclset/theorem/44")
;;       ("For Heyting Lattices)" "/item/openlatt/deftheorem/18"))) ; probably not accurate
;;     ("Tarski Axiom A" "/item/tarski/theorem/9")
;;     ("Tarski-Knaster Theorem" "/item/knaster/theorem/43")
;;     ("Taylor Expansion" "/article/taylor_1")
;;     ("The Short(est) Axiomatization of Orthomodular Ortholattices" "/item/robbins4/theorem/38")
;;     ("Tichonov Theorem" "/item/yellow17/theorem/24")
;;     ("Tietze Extension Theorem" "/item/tietze/theorem/25")
;;     ("Transfinite Induction" "/item/ordinal1/scheme/2")
;;     ("Unique Representation of Natural Numbers in Positional Numeral Systems" "/item/numeral1/fcluster/2") ; probably not accurate
;;     ("Urysohn Lemma" "/item/urysohn3/theorem/23")
;;     ("Wedderburn Theorem (Witt's proof)" "/item/weddwitt/theorem/39")
;;     ("Weierstrass Extreme Value Theorem" "/item/fcont_1/theorem/32")
;;     ("Weierstrass Theorem" "/item/weierstr/theorem/21")
;;     ("Yoneda Lemma" "/item/yoneda_1/theorem/9")
;;     ("Zassenhaus Lemma" "/item/group_9/theorem/93")
;;     ("Zermelo Theorem" "/item/wellord2/theorem/26")
;;     ("L'Hospital Rule" "/item/l_hospit/theorem/10"))
;; )