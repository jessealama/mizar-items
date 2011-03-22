
(in-package :mizar)

(define-constant
    +100-theorems+
    #1A
  ("The Irrationality of the Square Root of 2"
   "Fundamental Theorem of Algebra"
   "The Denumerability of the Rational Numbers"
   "Pythagorean Theorem"
   "Prime Number Theorem"
   "Gödel's Incompleteness Theorem"
   "Law of Quadratic Reciprocity"
   "The Impossibility of Trisecting the Angle and Doubling the Cube"
   "The Area of a Circle"
   "Euler's Generalization of Fermat's Little Theorem"
   "The Infinitude of Primes"
   "The Independence of the Parallel Postulate"
   "Polyhedron Formula"
   "Euler's Summation of 1 + (1/2)^2 + (1/3)^2 + ...."
   "Fundamental Theorem of Integral Calculus"
   "Insolvability of General Higher Degree Equations"
   "De Moivre's Theorem"
   "Liouville's Theorem and the Construction of Transcendental Numbers"
   "Four Squares Theorem"
   "All Primes (1 mod 4) Equal the Sum of Two Squares"
   "Green's Theorem"
   "The Non-Denumerability of the Continuum"
   "Formula for Pythagorean Triples"
   "The Undecidability of the Continuum Hypothesis"
   "Schroeder-Bernstein Theorem"
   "Leibnitz's Series for Pi"
   "Sum of the Angles of a Triangle"
   "Pascal's Hexagon Theorem"
   "Feuerbach's Theorem"
   "The Ballot Problem"
   "Ramsey's Theorem"
   "The Four Color Problem"
   "Fermat's Last Theorem"
   "Divergence of the Harmonic Series"
   "Taylor's Theorem"
   "Brouwer Fixed Point Theorem"
   "The Solution of a Cubic"
   "Arithmetic Mean/Geometric Mean"
   "Solutions to Pell's Equation"
   "Minkowski's Fundamental Theorem"
   "Puiseux's Theorem"
   "Sum of the Reciprocals of the Triangular Numbers"
   "The Isoperimetric Theorem"
   "The Binomial Theorem"
   "The Partition Theorem"
   "The Solution of the General Quartic Equation"
   "The Central Limit Theorem"
   "Dirichlet's Theorem"
   "The Cayley-Hamilton Theorem"
   "The Number of Platonic Solids"
   "Wilson's Theorem"
   "The Number of Subsets of a Set"
   "Pi is Transcendental"
   "Konigsberg Bridges Problem"
   "Product of Segments of Chords"
   "The Hermite-Lindemann Transcendence Theorem"
   "Heron's Formula"
   "Formula for the Number of Combinations"
   "The Laws of Large Numbers"
   "Bezout's Theorem"
   "Theorem of Ceva"
   "Fair Games Theorem"
   "Cantor's Theorem"
   "L'Hôpital's Rule"
   "Isosceles Triangle Theorem"
   "Sum of a Geometric Series"
   "e is Transcendental"
   "Sum of an arithmetic series"
   "Greatest Common Divisor Algorithm"
   "The Perfect Number Theorem"
   "Order of a Subgroup"
   "Sylow's Theorem"
   "Ascending or Descending Sequences"
   "The Principle of Mathematical Induction"
   "The Mean Value Theorem"
   "Fourier Series"
   "Sum of kth powers"
   "The Cauchy-Schwarz Inequality"
   "The Intermediate Value Theorem"
   "The Fundamental Theorem of Arithmetic"
   "Divergence of the Prime Reciprocal Series"
   "Dissection of Cubes (J.E. Littlewood's \"elegant\" proof)"
   "The Friendship Theorem"
   "Morley's Theorem"
   "Divisibility by 3 Rule"
   "Lebesgue Measure and Integration"
   "Desargues's Theorem"
   "Derangements Formula"
   "The Factor and Remainder Theorems"
   "Stirling's Formula"
   "The Triangle Inequality"
   "Pick's Theorem"
   "The Birthday Problem"
   "The Law of Cosines"
   "Ptolemy's Theorem"
   "Principle of Inclusion/Exclusion"
   "Cramer's Rule"
   "Bertrand's Postulate"
   "Buffon Needle Problem"
   "Descartes Rule of Signs"
   )
  :test #'equalp)

(define-constant mizar-formalized
    '(0 ("/irrat_1/theorem/1")
      1 ("polynom5/theorem/75")
      2 ("topgen_3/theorem/17")
      3 ("euclid_3/theorem/55")
      6 ("int_5/theorem/49")
      9 ("euler_2/theorem/33")
      10 ("newton/theorem/97")
      12 ("polyform/theorem/91")
      14 ("integra5/theorem/13")
      16 ("sin_cos3/theorem/51")
      19 ("nat_5/theorem/23")
      21 ("topgen_3/theorem/30")
      22 ("pythtrip/deftheorem/5")
      24 ("knaster/theorem/12")
      26 ("complex2/theorem/98")
      30 ("ramsey_1/theorem/15")
      33 ("series_1/theorem/37")
      34 ("taylor_1/theorem33")
      35 ("brouwer/theorem/14")
      43 ("binom/theorem/26")
      45 ("polyeq_5/theorem/30")
      50 ("nat_5/theorem/22")
      51 ("card_2/theorem/24")
      54 ("euclid_6/theorem/38")
      56 ("euclid_6/theorem/39")
      57 ("card_fin/theorem/18")
      59 ("newton/theorem/81")
      62 ("card_1/theorem/30")
      63 ("l_hospit/theorem/10")
      64 ("euclid_6/theorem/19")
      65 ("series_1/theorem/26")
      67 ("series_2/theorem/42")
      68 ("nat_1/scheme/8")
      69 ("nat_5/theorem/39")
      70 ("group_2/theorem/177")
      71 ("group_10/theorem/12" "group_10/theorem/14" "group_10/theorem/15")
      73 ("nat_1/scheme/1")
      74 ("rolle/theorem/3")
      77 ("hermitan/theorem/48")
      78 ("topreal5/theorem/10")
      79 ("nat_3/theorem/61" "int_7/theorem/15")
      84 ("numeral1/theorem/12")
      85 ("mesfunc5/deftheorem/16")
      88 ("uproots/theorem/52")
      90 ("euclid/theorem/16")
      95 ("card_fin/theorem/67")
      93 ("euclid_6/theorem/7")
      94 ("euclid_6/theorem/40")
      96 ("laplace/theorem/40"))
  :test #'equalp)
