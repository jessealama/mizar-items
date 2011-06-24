
(in-package :mizar)

(define-msc2010-classification 00-XX
    :name "00-XX"
    :description "General"
    :contains '(00-01 00-02 00Axx 00Bxx))

(define-msc2010-classification 01-XX
    :name "01-XX"
    :description "History and biography"
    :see-also nil)

;; should expand this when we know the full list of "-03 in the other sections" 
(define-msc2010-classification 03-XX
    :name "03-XX"
    :description "Mathematical logic and foundations"
    :see-also '("See also the classification number -03 in the other sections"))

(define-msc2010-classification 05-XX
    :name "05-XX"
    :description "Combinatorics"
    :see-also '(("Finite fields" 11T))
    :contains (05-00 05-01 05-02 05-03 05-04 05-06 05Axx 05Bxx 05Cxx 05Dxx 05Exx))

(define-msc2010-classification 06-XX
    :name "06-XX"
    :description "Order, lattices, ordered algebraic structures"
    :see-also '(18B35))

(define-msc2010-classification 08-XX
    :name "08-XX"
    :description "General algebraic systems"
    :contains (08-00 08-01 08-02 08-03 08-04 08-06 08Axx 08Bxx 08Cxx))

(define-msc201-classification 11-XX
    :name "11-XX"
    :description "Number theory"
    :contains (11-00 11-01 11-02 11-03 11-04 11-06 11Axx 11Bxx 11Cxx 11Dxx 11Exx 11Fxx 11Gxx 11Hxx 11Jxx 11Kxx 11Lxx 11Mxx 11Nxx 11Pxx 11Rxx 11Sxx 11Txx 11Uxx 11Yxx 11Zxx))

(define-msc2010-classification 12-XX
    :name "12-XX"
    :description "Field theory and polynomials")

(define-msc-2010-classification 13-XX
    :name "13-XX"
    :description "Commutative algebra")

(define-msc-2010-classification 14-XX
    :name "14-XX"
    :description "Algebraic geometry")

(define-msc-2010-classification 15-XX
    :name "15-XX"
    :description "Linear and multilinear algebra; matrix theory")

(define-msc-2010-classification 16-XX
    :name "16-XX"
    :description "Associative rings and algebras"
    :see-also '(("The commutative case" 13-XX)))

(define-msc-2010-classification 17-XX
    :name "17-XX"
    :description "Nonassociative rings and algebras")

(define-msc-2010-classification 18-XX
    :name "18-XX"
    :description "Category theory; homological algebra"
    :see-also '(("Commutative rings" 13Dxx)
		("Associative rings" 16Exx)
		("Groups" 20Jxx)
		("Topological groups and related structures" 57Txx)
		("Algebraic topology" 55Nxx 55Uxx)))

(define-msc-2010-classification 19-XX
    :name "19-XX"
    :description "$K$-theory"
    :see-also '(16E20 18F25))

(define-msc-2010-classification 20-XX
    :name "20-XX"
    :description "Group theory and generalizations")

(define-msc-2010-classification 22-XX
    :name "22-XX"
    :description "Topological groups, Lie groups"
    :see-also '(("Topological groups" 54H15 57Sxx 58-XX)
		("Abstract harmonic analysis" 43-XX)))

(define-msc-2010-classification 26-XX
    :name "26-XX"
    :description "Real functions"
    :see-also '(54C30))

(define-msc-2010-classification 28-XX
    :name "28-XX"
    :description "Measure and integration"
    :see-also '(("Analysis on manifolds" 58-XX)))

(define-msc-2010-classification 30-XX
    :name "30-XX"
    :description "Functions of a complex variable"
    :see-also '(("Analysis on manifolds" 58-XX)))

(define-msc-2010-classification 31-XX
    :name "31-XX"
    :description "Potential theory"
    :see-also '(("Probabilistic potential theory" 60J45})))

(define-msc-2010-classification 32-XX
    :name "32-XX"
    :description "Several complex variables and analytic spaces"
    :see-also '("For infinite-dimensional holomorphy" 46G20 58B12))

(define-msc-2010-classification 33-XX
    :name "33-XX"
    :description "Special functions"
    :see-also '(("Properties of functions as functions" 33-XX)
		("Orthogonal functions" 42Cxx)
		("Aspects of combinatorics" 05Axx)
		("Number-theoretic aspects" 11-XX)
		("Representation theory") 22Exx))

(define-msc-2010-classification 34-XX
    :name "34-XX"
    :description "Ordinary differential equations")

(define-msc-2010-classification 35-XX
    :name "35-XX"
    :description "Partial differential equations")

(define-msc-2010-classification 37-XX
    :name "37-XX"
    :description "Dynamical systems and ergodic theory"
    :see-also '(26A18 28Dxx 34Cxx 34Dxx 35Bxx 46Lxx 58Jxx 70-XX))

(define-msc-2010-classification 39-XX
    :name "39-XX"
    :description "Difference and functional equations")

(define-msc-2010-classification 40-XX
    :name "40-XX"
    :description "Sequences, series, summability")

(define-msc-2010-classification 41-XX
    :name "41-XX"
    :description "Approximations and expansions"
    :see-also '(("Approximation theory in the complex domain" 30E05 30E10)
		("Trigonometric approximation and interpolation" 42A10 42A15)
		("Numerical approximation" 65Dxx)))

(define-msc-2010-classification 42-XX
    :name "42-XX"
    :description "Harmonic analysis on Euclidean spaces Fourier analysis")

(define-msc-2010-classification 43-XX
    :name "43-XX"
    :description "Abstract harmonic analysis"
    :see-also '("Other analysis on topological and Lie groups" 22Exx))

(define-msc-2010-classification 44-XX
    :name "44-XX"
    :description "Integral transforms, operational calculus"
    :see-also '(("For fractional derivatives and integrals" 26A33)
		("Fourier transforms" 42A38 42B10)
		("Integral transforms in distribution spaces" 46F12)
		("Numerical methods" 65R10)))

(define-msc-2010-classification 45-XX
    :name "45-XX"
    :description "Integral equations")

(define-msc-2010-classification 46-XX
    :name "46-XX"
    :description "Functional analysis"
    :see-also (("Manifolds modeled on topological linear spaces" 57Nxx 58Bxx)))

(define-msc-2010-classification 47-XX
    :name "47-XX"
    :description "Operator theory")

(define-msc-2010-classification 49-XX
    :name "49-XX"
    :description "Calculus of variations and optimal control; optimization"
    :see-also '(34H05 34K35 65Kxx 90Cxx 93-XX))

(define-msc-2010-classification 51-XX
    :name "51-XX"
    :description "Geometry"
    :see-also '(("Algebraic geometry" 14-XX)))

(define-msc-2010-classification 52-XX
    :name "52-XX"
    :description "Convex and discrete geometry")

(define-msc-2010-classification 53-XX
    :name "53-XX"
    :description "Differential geometry"
    :see-also '(("Differential topology" 57Rxx)
		("Foundational questions of differentiable manifolds" 58Axx)))

(define-msc-2010-classification 54-XX
    :name "54-XX"
    :description "General topology"
    :see-also (("The topology of manifolds of all dimensions" 57Nxx)))

(define-msc-2010-classification 55-XX
    :name "55-XX"
    :description "Algebraic topology")

(define-msc-2010-classification 57-XX
    :name "57-XX"
    :description "Manifolds and cell complexes"
    :see-also '(("Complex manifolds" 32Qxx)))

(define-msc-2010-classification 58-XX
    :name "58-XX"
    :description "Global analysis, analysis on manifolds"
    :see-also '(("Geometric integration theory" 49Q15)
		32Cxx 32Fxx 32Wxx 46-XX 47Hxx 53Cxx))

(define-msc-2010-classification 60-XX
    :name "60-XX"
    :description "Probability theory and stochastic processes"
    :see-also '(("Applications" 11Kxx 62-XX 90-XX 91-XX 92-XX 93-XX 94-XX)))

(define-msc-2010-classification 62-XX
    :name "62-XX"
    :description "Statistics")

(define-msc-2010-classification 65-XX
    :name "65-XX"
    :description "Numerical analysis")

;; special case: refers to many subareas at once
(define-msc-2010-classification 68-XX
    :name "68-XX"
    :description "Computer science"
    :see-also '("Papers involving machine computations and programs in a specific mathematical area, see Section -04 in that area"))

(define-msc-2010-classification 70-XX
    :name "70-XX"
    :description "Mechanics of particles and systems"
    :see-also '(("Relativistic mechanics" 83A05 83C10)
		("Statistical mechanics" 82-XX)))

(define-msc-2010-classification 74-XX
    :name "74-XX"
    :description "Mechanics of deformable solids")

(define-msc-2010-classification 76-XX
    :name "76-XX"
    :description "Fluid mechanics"
    :see-also '(("General continuum mechanics" 74Axx 74-XX)))

(define-msc-2010-classification 78-XX
    :name "78-XX"
    :description "Optics, electromagnetic theory"
    :see-also '(("Quantum optics" 81V80)))

(define-msc-2010-classification 80-XX
    :name "80-XX"
    :description "Classical thermodynamics, heat transfer"
    :see-also '(("Thermodynamics of solids" 74A15)))

(define-msc-2010-classification 81-XX
    :name "81-XX"
    :description "Quantum theory")

(define-msc-2010-classification 82-XX
    :name "82-XX"
    :description "Statistical mechanics, structure of matter")

(define-msc-2010-classification 83-XX
    :name "83-XX"
    :description "Relativity and gravitational theory")

(define-msc-2010-classification 85-XX
    :name "85-XX"
    :description "Astronomy and astrophysics"
    :see-also '(("Celestial mechanics" 70F15)))

(define-msc-2010-classification 86-XX
    :name "86-XX"
    :description "Geophysics"
    :see-also '(76U05 76V05))

(define-msc-2010-classification 90-XX
    :name "90-XX"
    :description "Operations research, mathematical programming")

(define-msc-2010-classification 91-XX
    :name "91-XX"
    :description "Game theory, economics, social and behavioral sciences")

(define-msc-2010-classification 92-XX
    :name "92-XX"
    :description "Biology and other natural sciences")

(define-msc-2010-classification 93-XX
    :name "93-XX"
    :description "Systems theory; control"
    :see-also '(("Optimal control" 49-XX)))

(define-msc-2010-classification 94-XX
    :name "94-XX"
    :description "Information and communication, circuits")

(define-msc-2010-classification 97-XX
    :name "97-XX"
    :description "Mathematics education")

;; 00-XX

(define-msc-2010-classification 00-01
    :name "00-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 00-XX)

(define-msc-2010-classification 00-02
    :name "00-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 00-XX)

(define-msc-2010-classification 00Axx
    :name "00Axx"
    :description "General and miscellaneous specific topics"
    :contains '(00A05 00A06 00A07 00A08 00A09 00A15 00A17 00A20 00A22 00A30 00A35 00A65 00A66 00A67 00A69 00A71 00A72 00A73 00A79 00A99)
    :parent 00-XX)

(define-msc-2010-classification 00Bxx
    :name "00Bxx"
    :description "Conference proceedings and collections of papers"
    :contains '(00B05 00B10 00B15 00B20 00B25 00B30 00B50 00B55 00B60 00B99)
    :parent 00-XX)

;; OOAxx

(define-msc-2010-classification 00A05
    :name "00A05"
    :description "General mathematics"
    :parent 00Axx)

(define-msc-2010-classification 00A06
    :name "00A06"
    :description "Mathematics for nonmathematicians (engineering, social sciences, etc.)"
    :parent 00Axx)

(define-msc-2010-classification 00A07
    :name "00A07"
    :description "Problem books"
    :parent 00Axx)

(define-msc-2010-classification 00A08
    :name "00A08"
    :description "Recreational mathematics"
    :see-also '(97A20)
    :parent 00Axx)

(define-msc-2010-classification 00A09
    :name "00A09"
    :description "Popularization of mathematics"
    :parent 00Axx)

(define-msc-2010-classification 00A15
    :name "00A15"
    :description "Bibliographies"
    :parent 00Axx)

(define-msc-2010-classification 00A17
    :name "00A17"
    :description "External book reviews"
    :parent 00Axx)

(define-msc-2010-classification 00A20
    :name "00A20"
    :description "Dictionaries and other general reference works"
    :parent 00Axx)

(define-msc-2010-classification 00A22
    :name "00A22"
    :description "Formularies"
    :parent 00Axx)

(define-msc-2010-classification 00A30
    :name "00A30"
    :description "Philosophy of mathematics"
    :see-also '(03A05)
    :parent 00Axx)

(define-msc-2010-classification 00A35
    :name "00A35"
    :description "Methodology of mathematics, didactics"
    :see-also '(97Cxx 97Dxx)
    :parent 00Axx)

(define-msc-2010-classification 00A65
    :name "00A65"
    :description "Mathematics and music"
    :parent 00Axx)

(define-msc-2010-classification 00A66
    :name "00A66"
    :description "Mathematics and visual arts, visualization"
    :parent 00Axx)

(define-msc-2010-classification 00A67
    :name "00A67"
    :description "Mathematics and architecture"
    :parent 00Axx)

(define-msc-2010-classification 00A69
    :name "00A69"
    :description "General applied mathematics"
    :see-also '(("Physics" 00A79 70-XX 74-XX 76-XX 78-XX 80-XX 81-XX 82-XX 83-XX 85-XX 86-XX))
    :parent 00Axx)

(define-msc-2010-classification 00A71
    :name "00A71"
    :description "Theory of mathematical modeling"
    :parent 00Axx)

(define-msc-2010-classification 00A72
    :name "00A72"
    :description "General methods of simulation"
    :parent 00Axx)

(define-msc-2010-classification 00A73
    :name "00A73"
    :description "Dimensional analysis"
    :parent 00Axx)

(define-msc-2010-classification 00A79
    :name "00A79"
    :description "Physics"
    :see-also '((70-XX 74-XX 76-XX 78-XX 80-XX 81-XX 82-XX 83-XX 85-XX 86-XX))
    :parent 00Axx)

(define-msc-2010-classification 00A99
    :name "00A99"
    :description "Miscellaneous topics"
    :parent 00Axx)

;; 00Bxx

(define-msc-2010-classification 00B05
    :name "00B05"
    :description "Collections of abstracts of lectures"
    :parent 00Bxx)

(define-msc-2010-classification 00B10
    :name "00B10"
    :description "Collections of articles of general interest"
    :parent 00Bxx)

(define-msc-2010-classification 00B15
    :name "00B15"
    :description "Collections of articles of miscellaneous specific content"
    :parent 00Bxx)

(define-msc-2010-classification 00B20
    :name "00B20"
    :description "Proceedings of conferences of general interest"
    :parent 00Bxx)

(define-msc-2010-classification 00B25
    :name "00B25"
    :description "Proceedings of conferences of miscellaneous specific interest"
    :parent 00Bxx)

(define-msc-2010-classification 00B30
    :name "00B30"
    :description "Festschriften"
    :parent 00Bxx)

(define-msc-2010-classification 00B50
    :name "00B50"
    :description "Volumes of selected translations"
    :parent 00Bxx)

(define-msc-2010-classification 00B55
    :name "00B55"
    :description "Miscellaneous volumes of translations"
    :parent 00Bxx)

(define-msc-2010-classification 00B60
    :name "00B60"
    :description "Collections of reprinted articles"
    :see-also '(01A75)
    :parent 00Bxx)

(define-msc-2010-classification 00B99
    :name "00B99"
    :description "None of the above, but in this section"
    :parent 00Bxx)

;; 01-XX

(define-msc-2010-classification 01-00
    :name "01-00"
    :description "General reference works (handbooks, dictionaries, bibliographies, etc.)"
    :parent 01-XX)

(define-msc-2010-classification 01-01
    :name "01-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 01-XX)

(define-msc-2010-classification 01-02
    :name "01-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 01-XX)

(define-msc-2010-classification 01-06
    :name "01-06"
    :description "Proceedings, conferences, collections, etc."
    :parent 01-XX)

(define-msc-2010-classification 01-08
    :name "01-08"
    :description "Computational methods"
    :parent 01-XX)

(define-msc-2010-classification 01Axx
    :name "01Axx"
    :description "History of mathematics and mathematicians"
    :parent 01-XX)

;; 01Axx

(define-msc-2010-classification 01A05
    :name "01A05"
    :description "General histories, source books"
    :parent 01Axx)

(define-msc-2010-classification 01A07
    :name "01A07"
    :description "Ethnomathematics, general"
    :parent 01Axx)

(define-msc-2010-classification 01A10
    :name "01A10"
    :description "Paleolithic, Neolithic"
    :parent 01Axx)

(define-msc-2010-classification 01A12
    :name "01A12"
    :description "Indigenous cultures of the Americas"
    :parent 01Axx)

(define-msc-2010-classification 01A13
    :name "01A13"
    :description "Other indigenous cultures (non-European)"
    :parent 01Axx)

(define-msc-2010-classification 01A15
    :name "01A15"
    :description "Indigenous European cultures (pre-Greek, etc.)"
    :parent 01Axx)

(define-msc-2010-classification 01A16
    :name "01A16"
    :description "Egyptian"
    :parent 01Axx)

(define-msc-2010-classification 01A17
    :name "01A17"
    :description "Babylonian"
    :parent 01Axx)

(define-msc-2010-classification 01A20
    :name "01A20"
    :description "Greek, Roman"
    :parent 01Axx)

(define-msc-2010-classification 01A25
    :name "01A25"
    :description "China"
    :parent 01Axx)

(define-msc-2010-classification 01A27
    :name "01A27"
    :description "Japan"
    :parent 01Axx)

(define-msc-2010-classification 01A29
    :name "01A29"
    :description "Southeast Asia"
    :parent 01Axx)

(define-msc-2010-classification 01A30
    :name "01A30"
    :description "Islam (Medieval)"
    :parent 01Axx)

(define-msc-2010-classification 01A32
    :name "01A32"
    :description "India"
    :parent 01Axx)

(define-msc-2010-classification 01A35
    :name "01A35"
    :description "Medieval"
    :parent 01Axx)

(define-msc-2010-classification 01A40
    :name "01A40"
    :description "15th and 16th centuries, Renaissance"
    :parent 01Axx)

(define-msc-2010-classification 01A45
    :name "01A45"
    :description "17th century"
    :parent 01Axx)

(define-msc-2010-classification 01A50
    :name "01A50"
    :description "18th century"
    :parent 01Axx)

(define-msc-2010-classification 01A55
    :name "01A55"
    :description "19th century"
    :parent 01Axx)

(define-msc-2010-classification 01A60
    :name "01A60"
    :description "20th century"
    :parent 01Axx)

(define-msc-2010-classification 01A61
    :name "01A61"
    :description "Twenty-first century"
    :parent 01Axx)

(define-msc-2010-classification 01A65
    :name "01A65"
    :description "Contemporary"
    :parent 01Axx)

(define-msc-2010-classification 01A67
    :name "01A67"
    :description "Future prospectives"
    :parent 01Axx)

(define-msc-2010-classification 01A70
    :name "01A70"
    :description "Biographies, obituaries, personalia, bibliographies"
    :parent 01Axx)

(define-msc-2010-classification 01A72
    :name "01A72"
    :description "Schools of mathematics"
    :parent 01Axx)

(define-msc-2010-classification 01A73
    :name "01A73"
    :description "Universities"
    :parent 01Axx)

(define-msc-2010-classification 01A74
    :name "01A74"
    :description "Other institutions and academies"
    :parent 01Axx)

(define-msc-2010-classification 01A75
    :name "01A75"
    :description "Collected or selected works; reprintings or translations of classics"
    :see-also '(00B60)
    :parent 01Axx)

(define-msc-2010-classification 01A80
    :name "01A80"
    :description "Sociology (and profession) of mathematics"
    :parent 01Axx)

(define-msc-2010-classification 01A85
    :name "01A85"
    :description "Historiography"
    :parent 01Axx)

(define-msc-2010-classification 01A90
    :name "01A90"
    :description "Bibliographic studies"
    :parent 01Axx)

(define-msc-2010-classification 01A99
    :name "01A99"
    :description "Miscellaneous topics"
    :parent 01Axx)

;; 03-XX

(define-msc-2010-classification 03-00
    :name "03-00"
    :description "General reference works (handbooks, dictionaries, bibliographies, etc.)"
    :parent 03-XX)

(define-msc-2010-classification 03-01
    :name "03-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 03-XX)

(define-msc-2010-classification 03-02
    :name "03-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 03-XX)

(define-msc-2010-classification 03-03
    :name "03-03"
    :description "Historical"
    :see-also '(01-XX)
    :parent 03-XX)

(define-msc-2010-classification 03-04
    :name "03-04"
    :description "Explicit machine computation and programs (not the theory of computation or programming)"
    :parent 03-XX)

(define-msc-2010-classification 03-06
    :name "03-06"
    :description "Proceedings, conferences, collections, etc."
    :parent 03-XX)

(define-msc-2010-classification 03Axx
    :name "03Axx"
    :description "Philosophical aspects of logic and foundations"
    :parent 03-XX
    :contains (03A05 03A10 03A99))

(define-msc-2010-classification 03Bxx
    :name "03Bxx"
    :description "General logic"
    :parent 03-XX
    :contains (03B05 03B10 03B15 03B20 03B22 03B25 03B30 03B35 03B40 03B42 03B44 03B45 03B47 03B48 03B50 03B52 03B53 03B55 03B60 03B62 03B65 03B70 03B80 03B99))

(define-msc-2010-classification 03Cxx
    :name "03Cxx"
    :description "Model theory"
    :parent 03-XX
    :contains (03C05 03C07 03C10 03C13 03C15 03C20 03C25 03C30 03C35 03C40 03C45 03C48 03C50 03C52 03C55 03C57 03C60 03C62 03C64 03C65 03C68 03C70 03C75 03C80 03C85 03C90 03C95 03C98 03C99))

(define-msc-2010-classification 03Dxx
    :name "03Dxx"
    :description "Computability and recursion theory"
    :parent 03-XX
    :contains (03D03 03D05 03D10 03D15 03D20 03D25 03D28 03D30 03D32 03D35 03D40 03D45 03D50 03D55 03D60 03D65 03D70 03D75 03D78 03D80 03D99))

(define-msc-2010-classification 03Exx
    :name "03Exx"
    :description "Set theory"
    :parent 03-XX
    :contains (03E02 03E04 03E05 03E10 03E15 03E17 03E20 03E25 03E30 03E35 03E40 03E45 03E47 03E50 03E55 03E57 03E60 03E65 03E70 03E72 03E75 03E99))

(define-msc-2010-classification 03Fxx
    :name "03Fxx"
    :description "Proof theory and constructive mathematics"
    :parent 03-XX
    :contains (03F03 03F05 03F07 03F10 03F15 03F20 03F25 03F30 03F35 03F40 03F45 03F50 03F52 03F55 03F60 03F65 03F99))

(define-msc-2010-classification 03Gxx
    :name "03Gxx"
    :description "Algebraic logic"
    :parent 03-XX
    :contains (03G05 03G10 03G12 03G15 03G20 03G25 03G27 03G30 03G99))

(define-msc-2010-classification 03Hxx
    :name "03Hxx"
    :description "Nonstandard models"
    :see-also '(03C62)
    :parent 03-XX
    :contains (03H05 03H10 03H15 03H99))

;; 03Axx

(define-msc-2010-classification 03A05
    :name "03A05"
    :description "Philosophical and critical"
    :see-also '(("Philosophy of mathematics" 00A30))
    :parent 03Axx)

(define-msc-2010-classification 03A10
    :name "03A10"
    :description "Logic in the philosophy of science"
    :parent 03Axx)

(define-msc-2010-classification 03A99
    :name "03A99"
    :description "None of the above, but in this section"
    :parent 03Axx)

;; 03Bxx

(define-msc-2010-classification 03B05
    :name "03B05"
    :description "Classical propositional logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B10
    :name "03B10"
    :description "Classical first-order logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B15
    :name "03B15"
    :description "Higher-order logic and type theory"
    :parent 03Bxx)

(define-msc-2010-classification 03B20
    :name "03B20"
    :description "Subsystems of classical logic (including intuitionistic logic)"
    :parent 03Bxx)

(define-msc-2010-classification 03B22
    :name "03B22"
    :description "Abstract deductive systems"
    :parent 03Bxx)

(define-msc-2010-classification 03B25
    :name "03B25"
    :description "Decidability of theories and sets of sentences"
    :see-also '(11U05 12L05 20F10)
    :parent 03Bxx)

(define-msc-2010-classification 03B30
    :name "03B30"
    :description "Foundations of classical theories (including reverse mathematics)"
    :see-also '(03F35)
    :parent 03Bxx)

(define-msc-2010-classification 03B35
    :name "03B35"
    :description "Mechanization of proofs and logical operations"
    :see-also '(68T15)
    :parent 03Bxx)

(define-msc-2010-classification 03B40
    :name "03B40"
    :description "Combinatory logic and lambda-calculus"
    :see-also '(68N18)
    :parent 03Bxx)

(define-msc-2010-classification 03B42
    :name "03B42"
    :description "Logics of knowledge and belief (including belief change)"
    :parent 03Bxx)

(define-msc-2010-classification 03B44
    :name "03B44"
    :description "Temporal logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B45
    :name "03B45"
    :description "Modal logic (including the logic of norms)"
    :see-also '(("Knowledge and belief" 03B42)
		("Temporal logic" 03B44)
		("Provability logic" 03F45))
    :parent 03Bxx)

(define-msc-2010-classification 03B47
    :name "03B47"
    :description "Substructural logics (including relevance, entailment, linear logic, Lambek calculus, BCK and BCI logics)"
    :see-also '(("Proof-theoretic aspects" 03F52))
    :parent 03Bxx)

(define-msc-2010-classification 03B48
    :name "03B48"
    :description "Probability and inductive logic"
    :see-also '(60A05)
    :parent 03Bxx)

(define-msc-2010-classification 03B50
    :name "03B50"
    :description "Many-valued logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B52
    :name "03B52"
    :description "Fuzzy logic; logic of vagueness"
    :see-also '(68T27 68T37 94D05)
    :parent 03Bxx)

(define-msc-2010-classification 03B53
    :name "03B53"
    :description "Paraconsistent logics"
    :parent 03Bxx)

(define-msc-2010-classification 03B55
    :name "03B55"
    :description "Intermediate logics"
    :parent 03Bxx)

(define-msc-2010-classification 03B60
    :name "03B60"
    :description "Other nonclassical logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B62
    :name "03B62"
    :description "Combined logics"
    :parent 03Bxx)

(define-msc-2010-classification 03B65
    :name "03B65"
    :description "Logic of natural languages"
    :see-also '(68T50 91F20)
    :parent 03Bxx)

(define-msc-2010-classification 03B70
    :name "03B70"
    :description "Logic in computer science"
    :see-also '(68-XX)
    :parent 03Bxx)

(define-msc-2010-classification 03B80
    :name "03B80"
    :description "Other applications of logic"
    :parent 03Bxx)

(define-msc-2010-classification 03B99
    :name "03B99"
    :description "None of the above, but in this section"
    :parent 03Bxx)

;; 03Cxx

(define-msc-2010-classification 03C05
    :name "03C05"
    :description "Equational classes, universal algebra"
    :see-also '(08Axx 08Bxx 18C05)
    :parent 03Cxx)

(define-msc-2010-classification 03C07
    :name "03C07"
    :description "Basic properties of first-order languages and structures"
    :parent 03Cxx)

(define-msc-2010-classification 03C10
    :name "03C10"
    :description "Quantifier elimination, model completeness and related topics"
    :parent 03Cxx)

(define-msc-2010-classification 03C13
    :name "03C13"
    :description "Finite structures"
    :see-also '(68Q15 68Q19])
    :parent 03Cxx)

(define-msc-2010-classification 03C15
    :name "03C15"
    :description "Denumerable structures"
    :parent 03Cxx)

(define-msc-2010-classification 03C20
    :name "03C20"
    :description "Ultraproducts and related constructions"
    :parent 03Cxx)

(define-msc-2010-classification 03C25
    :name "03C25"
    :description "Model-theoretic forcing"
    :parent 03Cxx)

(define-msc-2010-classification 03C30
    :name "03C30"
    :description "Other model constructions"
    :parent 03Cxx)

(define-msc-2010-classification 03C35
    :name "03C35"
    :description "Categoricity and completeness of theories"
    :parent 03Cxx)

(define-msc-2010-classification 03C40
    :name "03C40"
    :description "Interpolation, preservation, definability"
    :parent 03Cxx)

(define-msc-2010-classification 03C45
    :name "03C45"
    :description "Classification theory, stability and related concepts"
    :see-also '(03C48)
    :parent 03Cxx)

(define-msc-2010-classification 03C48
    :name "03C48"
    :description "Abstract elementary classes and related topics"
    :see-also '(03C45)
    :parent 03Cxx)

(define-msc-2010-classification 03C50
    :name "03C50"
    :description "Models with special properties (saturated, rigid, etc.)"
    :parent 03Cxx)

(define-msc-2010-classification 03C52
    :name "03C52"
    :description "Properties of classes of models"
    :parent 03Cxx)

(define-msc-2010-classification 03C55
    :name "03C55"
    :description "Set-theoretic model theory"
    :parent 03Cxx)

(define-msc-2010-classification 03C57
    :name "03C57"
    :description "Effective and recursion-theoretic model theory"
    :see-also '(03D45)
    :parent 03Cxx)

(define-msc-2010-classification 03C60
    :name "03C60"
    :description "Model-theoretic algebra"
    :see-also '(08C10 12Lxx 13L05)
    :parent 03Cxx)

(define-msc-2010-classification 03C62
    :name "03C62"
    :description "Models of arithmetic and set theory"
    :see-also '(03Hxx)
    :parent 03Cxx)

(define-msc-2010-classification 03C64
    :name "03C64"
    :description "Model theory of ordered structures; o-minimality"
    :parent 03Cxx)

(define-msc-2010-classification 03C65
    :name "03C65"
    :description "Models of other mathematical theories"
    :parent 03Cxx)

(define-msc-2010-classification 03C68
    :name "03C68"
    :description "Other classical first-order model theory"
    :parent 03Cxx)

(define-msc-2010-classification 03C70
    :name "03C70"
    :description "Logic on admissible sets"
    :parent 03Cxx)

(define-msc-2010-classification 03C75
    :name "03C75"
    :description "Other infinitary logic"
    :parent 03Cxx)

(define-msc-2010-classification 03C80
    :name "03C80"
    :description "Logic with extra quantifiers and operators"
    :see-also '(03B42 03B44 03B45 03B48)
    :parent 03Cxx)

(define-msc-2010-classification 03C85
    :name "03C85"
    :description "Second- and higher-order model theory"
    :parent 03Cxx)

(define-msc-2010-classification 03C90
    :name "03C90"
    :description "Nonclassical models (Boolean-valued, sheaf, etc.)"
    :parent 03Cxx)

(define-msc-2010-classification 03C95
    :name "03C95"
    :description "Abstract model theory"
    :parent 03Cxx)

(define-msc-2010-classification 03C98
    :name "03C98"
    :description "Applications of model theory"
    :see-also '(03C60)
    :parent 03Cxx)

(define-msc-2010-classification 03C99
    :name "03C99"
    :description "None of the above, but in this section"
    :parent 03Cxx)

;; 03Dxx

(define-msc-2010-classification 03D03
    :name 03D03
    :description "Thue and Post systems, etc."
    :parent 03Dxx)

(define-msc-2010-classification 03D05
    :name 03D05
    :description "Automata and formal grammars in connection with logical questions"
    :see-also '(68Q45 68Q70 68R15)
    :parent 03Dxx)

(define-msc-2010-classification 03D10
    :name 03D10
    :description "Turing machines and related notions"
    :see-also '(68Q05)
    :parent 03Dxx)

(define-msc-2010-classification 03D15
    :name 03D15
    :description "Complexity of computation (including implicit computational complexity)"
    :see-also '(68Q15 68Q17)
    :parent 03Dxx)

(define-msc-2010-classification 03D20
    :name 03D20
    :description "Recursive functions and relations, subrecursive hierarchies"
    :parent 03Dxx)

(define-msc-2010-classification 03D25
    :name 03D25
    :description "Recursively (computably) enumerable sets and degrees"
    :parent 03Dxx)

(define-msc-2010-classification 03D28
    :name 03D28
    :description "Other Turing degree structures"
    :parent 03Dxx)

(define-msc-2010-classification 03D30
    :name 03D30
    :description "Other degrees and reducibilities"
    :parent 03Dxx)

(define-msc-2010-classification 03D32
    :name 03D32
    :description "Algorithmic randomness and dimension"
    :see-also '(68Q30)
    :parent 03Dxx)

(define-msc-2010-classification 03D35
    :name 03D35
    :description "Undecidability and degrees of sets of sentences"
    :parent 03Dxx)

(define-msc-2010-classification 03D40
    :name 03D40
    :description "Word problems, etc."
    :see-also '(06B25 08A50 20F10 68R15)
    :parent 03Dxx)

(define-msc-2010-classification 03D45
    :name 03D45
    :description "Theory of numerations, effectively presented structures"
    :see-also '(("Intuitionistic and similar approaches" 03F55)
		03C57)
    :parent 03Dxx)

(define-msc-2010-classification 03D50
    :name 03D50
    :description "Recursive equivalence types of sets and structures, isols"
    :parent 03Dxx)

(define-msc-2010-classification 03D55
    :name 03D55
    :description "Hierarchies"
    :parent 03Dxx)

(define-msc-2010-classification 03D60
    :name 03D60
    :description "Computability and recursion theory on ordinals, admissible sets, etc."
    :parent 03Dxx)

(define-msc-2010-classification 03D65
    :name 03D65
    :description "Higher-type and set recursion theory"
    :parent 03Dxx)

(define-msc-2010-classification 03D70
    :name 03D70
    :description "Inductive definability"
    :parent 03Dxx)

(define-msc-2010-classification 03D75
    :name 03D75
    :description "Abstract and axiomatic computability and recursion theory"
    :parent 03Dxx)

(define-msc-2010-classification 03D78
    :name 03D78
    :description "Computation over the reals"
    :see-also '(("Constructive aspects" 03F60))
    :parent 03Dxx)

(define-msc-2010-classification 03D80
    :name 03D80
    :description "Applications of computability and recursion theory"
    :parent 03Dxx)

(define-msc-2010-classification 03D99
    :name 03D99
    :description "None of the above, but in this section"
    :parent 03Dxx)

;; 03Exx

(define-msc-2010-classification 03E02
    :name "03E02"
    :description "Partition relations"
    :parent 03Exx)

(define-msc-2010-classification 03E04
    :name "03E04"
    :description "Ordered sets and their cofinalities; pcf theory"
    :parent 03Exx)

(define-msc-2010-classification 03E05
    :name "03E05"
    :description "Other combinatorial set theory"
    :parent 03Exx)

(define-msc-2010-classification 03E10
    :name "03E10"
    :description "Ordinal and cardinal numbers"
    :parent 03Exx)

(define-msc-2010-classification 03E15
    :name "03E15"
    :description "Descriptive set theory"
    :see-also '(28A05 54H05)
    :parent 03Exx)

(define-msc-2010-classification 03E17
    :name "03E17"
    :description "Cardinal characteristics of the continuum"
    :parent 03Exx)

(define-msc-2010-classification 03E20
    :name "03E20"
    :description "Other classical set theory (including functions, relations, and set algebra)"
    :parent 03Exx)

(define-msc-2010-classification 03E25
    :name "03E25"
    :description "Axiom of choice and related propositions"
    :parent 03Exx)

(define-msc-2010-classification 03E30
    :name "03E30"
    :description "Axiomatics of classical set theory and its fragments"
    :parent 03Exx)

(define-msc-2010-classification 03E35
    :name "03E35"
    :description "Consistency and independence results"
    :parent 03Exx)

(define-msc-2010-classification 03E40
    :name "03E40"
    :description "Other aspects of forcing and Boolean-valued models"
    :parent 03Exx)

(define-msc-2010-classification 03E45
    :name "03E45"
    :description "Inner models, including constructibility, ordinal definability, and core models"
    :parent 03Exx)

(define-msc-2010-classification 03E47
    :name "03E47"
    :description "Other notions of set-theoretic definability"
    :parent 03Exx)

(define-msc-2010-classification 03E50
    :name "03E50"
    :description "Continuum hypothesis and Martin's axiom"
    :see-also '(03E57)
    :parent 03Exx)

(define-msc-2010-classification 03E55
    :name "03E55"
    :description "Large cardinals"
    :parent 03Exx)

(define-msc-2010-classification 03E57
    :name "03E57"
    :description "Generic absoluteness and forcing axioms"
    :see-also '(03E50)
    :parent 03Exx)

(define-msc-2010-classification 03E60
    :name "03E60"
    :description "Determinacy principles"
    :parent 03Exx)

(define-msc-2010-classification 03E65
    :name "03E65"
    :description "Other hypotheses and axioms"
    :parent 03Exx)

(define-msc-2010-classification 03E70
    :name "03E70"
    :description "Nonclassical and second-order set theories"
    :parent 03Exx)

(define-msc-2010-classification 03E72
    :name "03E72"
    :description "Fuzzy set theory"
    :parent 03Exx)

(define-msc-2010-classification 03E75
    :name "03E75"
    :description "Applications of set theory"
    :parent 03Exx)

(define-msc-2010-classification 03E99
    :name "03E99"
    :description "None of the above, but in this section"
    :parent 03Exx)

;; 03Fxx

(define-msc-2010-classification 03F03
    :name "03F03"
    :description "Proof theory, general"
    :parent 03Fxx)

(define-msc-2010-classification 03F05
    :name "03F05"
    :description "Cut-elimination and normal-form theorems"
    :parent 03Fxx)

(define-msc-2010-classification 03F07
    :name "03F07"
    :description "Structure of proofs"
    :parent 03Fxx)

(define-msc-2010-classification 03F10
    :name "03F10"
    :description "Functionals in proof theory"
    :parent 03Fxx)

(define-msc-2010-classification 03F15
    :name "03F15"
    :description "Recursive ordinals and ordinal notations"
    :parent 03Fxx)

(define-msc-2010-classification 03F20
    :name "03F20"
    :description "Complexity of proofs"
    :parent 03Fxx)

(define-msc-2010-classification 03F25
    :name "03F25"
    :description "Relative consistency and interpretations"
    :parent 03Fxx)

(define-msc-2010-classification 03F30
    :name "03F30"
    :description "First-order arithmetic and fragments"
    :parent 03Fxx)

(define-msc-2010-classification 03F35
    :name "03F35"
    :description "Second- and higher-order arithmetic and fragments"
    :see-also '(03B30)
    :parent 03Fxx)

(define-msc-2010-classification 03F40
    :name "03F40"
    :description "Gödel numberings and issues of incompleteness"
    :parent 03Fxx)

(define-msc-2010-classification 03F45
    :name "03F45"
    :description "Provability logics and related algebras (e.g., diagonalizable algebras)"
    :see-also '(03B45 03G25 06E25)
    :parent 03Fxx)

(define-msc-2010-classification 03F50
    :name "03F50"
    :description "Metamathematics of constructive systems"
    :parent 03Fxx)

(define-msc-2010-classification 03F52
    :name "03F52"
    :description "Linear logic and other substructural logics"
    :see-also '(03B47)
    :parent 03Fxx)

(define-msc-2010-classification 03F55
    :name "03F55"
    :description "Intuitionistic mathematics"
    :parent 03Fxx)

(define-msc-2010-classification 03F60
    :name "03F60"
    :description "Constructive and recursive analysis"
    :see-also '(03B30 03D45 03D78 26E40 46S30 47S30)
    :parent 03Fxx)

(define-msc-2010-classification 03F65
    :name "03F65"
    :description "Other constructive mathematics"
    :see-also '(03D45)
    :parent 03Fxx)

(define-msc-2010-classification 03F99
    :name "03F99"
    :description "None of the above, but in this section"
    :parent 03Fxx)

;; 03Gxx

(define-msc-2010-classification 03G05
    :name "03G05"
    :description "Boolean algebras"
    :see-also '(06Exx)
    :parent 03Gxx)

(define-msc-2010-classification 03G10
    :name "03G10"
    :description "Lattices and related structures"
    :see-also '(06Bxx)
    :parent 03Gxx)

(define-msc-2010-classification 03G12
    :name "03G12"
    :description "Quantum logic"
    :see-also '(06C15 81P10)
    :parent 03Gxx)

(define-msc-2010-classification 03G15
    :name "03G15"
    :description "Cylindric and polyadic algebras; relation algebras"
    :parent 03Gxx)

(define-msc-2010-classification 03G20
    :name "03G20"
    :description "Łukasiewicz and Post algebras"
    :see-also '(06D25 06D30)
    :parent 03Gxx)

(define-msc-2010-classification 03G25
    :name "03G25"
    :description "Other algebras related to logic"
    :see-also '(03F45 06D20 06E25 06F35)
    :parent 03Gxx)

(define-msc-2010-classification 03G27
    :name "03G27"
    :description "Abstract algebraic logic"
    :parent 03Gxx)

(define-msc-2010-classification 03G30
    :name "03G30"
    :description "Categorical logic, topoi"
    :see-also '(18B25 18C05 18C10)
    :parent 03Gxx)

(define-msc-2010-classification 03G99
    :name "03G99"
    :description "None of the above, but in this section"
    :parent 03Gxx)

;; 03Hxx

(define-msc-2010-classification 03H05
    :name "03H05"
    :description "Nonstandard models in mathematics"
    :see-also '(26E35 28E05 30G06 46S20 47S20 54J05)
    :paent 03Hxx)

(define-msc-2010-classification 03H10
    :name "03H10"
    :description "Other applications of nonstandard models (economics, physics, etc.)"
    :paent 03Hxx)

(define-msc-2010-classification 03H15
    :name "03H15"
    :description "Nonstandard models of arithmetic"
    :see-also '(11U10 12L15 13L05)
    :paent 03Hxx)

(define-msc-2010-classification 03H99
    :name "03H99"
    :description "None of the above, but in this section"
    :paent 03Hxx)

;; 05-XX

(define-msc-2010-classification 05-00
    :name "05-00"
    :description "General reference works (handbooks, dictionaries, bibliographies, etc.)"
    :parent 05-XX) 

(define-msc-2010-classification 05-01
    :name "05-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 05-XX) 

(define-msc-2010-classification 05-02
    :name "05-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 05-XX) 

(define-msc-2010-classification 05-03
    :name "05-03"
    :description "Historical (must also be assigned at least one classification number from Section 01)"
    :parent 05-XX) 

(define-msc-2010-classification 05-04
    :name "05-04"
    :description "Explicit machine computation and programs (not the theory of computation or programming)"
    :parent 05-XX) 

(define-msc-2010-classification 05-06
    :name "05-06"
    :description "Proceedings, conferences, collections, etc."
    :parent 05-XX) 

(define-msc-2010-classification 05Axx
    :name "05Axx"
    :description "Enumerative combinatorics"
    :see-also '(("Enumeration in graph theory" 05C30))
    :parent 05-XX
    :contains (05A05 05A10 05A15 05A16 05A17 05A18 05A19 05A20 05A30 05A40 05A99)) 

(define-msc-2010-classification 05Bxx
    :name "05Bxx"
    :description "Designs and configurations"
    :see-also (("Applications of design theory" 94C30))
    :parent 05-XX
    :contains (05B05 05B07 05B10 05B15 05B20 05B25 05B30 05B35 05B40 05B45 05B50 05B99)) 

(define-msc-2010-classification 05Cxx
    :name "05Cxx"
    :description "Graph theory"
    :see-also (("Applications of graphs" 68R10 81Q30 81T15 82B20 82C20 90C35 92E10 94C15))
    :parent 05-XX
    :contains (05C05 05C07 05C10 05C12 05C15 05C17 05C20 05C21 05C22 05C25 05C30 05C31 05C35 05C38 05C40 05C42 05C45 05C50 05C51 05C55 05C57 05C60 05C62 05C63 05C65 05C69 05C70 05C72 05C75 05C76 05C78 05C80 05C81 05C82 05C83 05C85 05C90 05C99)) 

(define-msc-2010-classification 05Dxx
    :name "05Dxx"
    :description "Extremal combinatorics"
    :parent 05-XX
    :contains (05D05 05D10 05D15 05D40 05D99)) 

(define-msc-2010-classification 05Exx
    :name "05Exx"
    :description "Algebraic combinatorics"
    :parent 05-XX
    :contains (05E05 05E10 05E15 05E18 05E30 05E40 05E45 05E99)) 

;; 05Axx

(define-msc-2010-classification 05A05
    :name "05A05"
    :description "Permutations, words, matrices"
    :parent 05Axx)

(define-msc-2010-classification 05A10
    :name "05A10"
    :description "Factorials, binomial coefficients, combinatorial functions"
    :see-also (11B65 33Cxx)
    :parent 05Axx)

(define-msc-2010-classification 05A15
    :name "05A15"
    :description "Exact enumeration problems, generating functions"
    :see-also '(33Cxx 33Dxx)
    :parent 05Axx)

(define-msc-2010-classification 05A16
    :name "05A16"
    :description "Asymptotic enumeration"
    :parent 05Axx)

(define-msc-2010-classification 05A17
    :name "05A17"
    :description "Partitions of integers"
    :see-also (11P81 11P82 11P83)
    :parent 05Axx)

(define-msc-2010-classification 05A18
    :name "05A18"
    :description "Partitions of sets"
    :parent 05Axx)

(define-msc-2010-classification 05A19
    :name "05A19"
    :description "Bijective combinatorics"
    :parent 05Axx)

(define-msc-2010-classification 05A20
    :name "05A20"
    :description "Combinatorial inequalities"
    :parent 05Axx)

(define-msc-2010-classification 05A30
    :name "05A30"
    :description "$q$-calculus and related topics"
    :see-also (33Dxx)
    :parent 05Axx)

(define-msc-2010-classification 05A40
    :name "05A40"
    :description "Umbral calculus"
    :parent 05Axx)

(define-msc-2010-classification 05A99
    :name "05A99"
    :description "None of the above, but in this section"
    :parent 05Axx)

;; 05Bxx

(define-msc-2010-classification 05B05
    :name "05B05"
    :description "Block designs"
    :see-also '(51E05 62K10)
    :parent 05Bxx)

(define-msc-2010-classification 05B07
    :name "05B07"
    :description "Triple systems"
    :parent 05Bxx)

(define-msc-2010-classification 05B10
    :name "05B10"
    :description "Difference sets (number-theoretic, group-theoretic, etc.)"
    :see-also '(11B13)
    :parent 05Bxx)

(define-msc-2010-classification 05B15
    :name "05B15"
    :description "Orthogonal arrays, Latin squares, Room squares"
    :parent 05Bxx)

(define-msc-2010-classification 05B20
    :name "05B20"
    :description "Matrices (incidence, Hadamard, etc.)"
    :parent 05Bxx)

(define-msc-2010-classification 05B25
    :name "05B25"
    :description "Finite geometries"
    :see-also '(51D20 51Exx)
    :parent 05Bxx)

(define-msc-2010-classification 05B30
    :name "05B30"
    :description "Other designs, configurations"
    :see-also '(51E30)
    :parent 05Bxx)

(define-msc-2010-classification 05B35
    :name "05B35"
    :description "Matroids, geometric lattices"
    :see-also '(52B40 90C27)
    :parent 05Bxx)

(define-msc-2010-classification 05B40
    :name "05B40"
    :description "Packing and covering"
    :see-also '(11H31 52C15 52C17)
    :parent 05Bxx)

(define-msc-2010-classification 05B45
    :name "05B45"
    :description "Tessellation and tiling problems"
    :see-also '(52C20 52C22)
    :parent 05Bxx)

(define-msc-2010-classification 05B50
    :name "05B50"
    :description "Polyominoes"
    :parent 05Bxx)

(define-msc-2010-classification 05B99
    :name "05B99"
    :description "None of the above, but in this section"
    :parent 05Bxx)

;; 05Cxx

(define-msc-2010-classification 05C05
    :name "05C05"
    :description "Trees"
    :parent 05Cxx)

(define-msc-2010-classification 05C07
    :name "05C07"
    :description "Vertex degrees"
    :see-also '(05E30)
    :parent 05Cxx)

(define-msc-2010-classification 05C10
    :name "05C10"
    :description "Planar graphs; geometric and topological aspects of graph theory"
    :see-also '(57M15 57M25)
    :parent 05Cxx)

(define-msc-2010-classification 05C12
    :name "05C12"
    :description "Distance in graphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C15
    :name "05C15"
    :description "Coloring of graphs and hypergraphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C17
    :name "05C17"
    :description "Perfect graphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C20
    :name "05C20"
    :description "Directed graphs (digraphs), tournaments"
    :parent 05Cxx)

(define-msc-2010-classification 05C21
    :name "05C21"
    :description "Flows in graphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C22
    :name "05C22"
    :description "Signed and weighted graphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C25
    :name "05C25"
    :description "Graphs and abstract algebra (groups, rings, fields, etc.)"
    :see-also '(20F65)
    :parent 05Cxx)

(define-msc-2010-classification 05C30
    :name "05C30"
    :description "Enumeration in graph theory"
    :parent 05Cxx)

(define-msc-2010-classification 05C31
    :name "05C31"
    :description "Graph polynomials"
    :parent 05Cxx)

(define-msc-2010-classification 05C35
    :name "05C35"
    :description "Extremal problems"
    :see-also '(90C35)
    :parent 05Cxx)

(define-msc-2010-classification 05C38
    :name "05C38"
    :description "Paths and cycles"
    :see-also '(90B10)
    :parent 05Cxx)

(define-msc-2010-classification 05C40
    :name "05C40"
    :description "Connectivity"
    :parent 05Cxx)

(define-msc-2010-classification 05C42
    :name "05C42"
    :description "Density (toughness, etc.)"
    :parent 05Cxx)

(define-msc-2010-classification 05C45
    :name "05C45"
    :description "Eulerian and Hamiltonian graphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C50
    :name "05C50"
    :description "Graphs and linear algebra (matrices, eigenvalues, etc.)"
    :parent 05Cxx)

(define-msc-2010-classification 05C51
    :name "05C51"
    :description "Graph designs and isomomorphic decomposition"
    :see-also '(05B30)
    :parent 05Cxx)

(define-msc-2010-classification 05C55
    :name "05C55"
    :description "Generalized Ramsey theory"
    :see-also '(05D10)
    :parent 05Cxx)

(define-msc-2010-classification 05C57
    :name "05C57"
    :description "Games on graphs"
    :see-also '(91A43 91A46)
    :parent 05Cxx)

(define-msc-2010-classification 05C60
    :name "05C60"
    :description "Isomorphism problems (reconstruction conjecture, etc.) and homomorphisms (subgraph embedding, etc.)"
    :parent 05Cxx)

(define-msc-2010-classification 05C62
    :name "05C62"
    :description "Graph representations (geometric and intersection representations, etc.)"
    :see-also '(("Graph drawing" 68R10))
    :parent 05Cxx)

(define-msc-2010-classification 05C63
    :name "05C63"
    :description "Infinite graphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C65
    :name "05C65"
    :description "Hypergraphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C69
    :name "05C69"
    :description "Dominating sets, independent sets, cliques"
    :parent 05Cxx)

(define-msc-2010-classification 05C70
    :name "05C70"
    :description "Factorization, matching, partitioning, covering and packing"
    :parent 05Cxx)

(define-msc-2010-classification 05C72
    :name "05C72"
    :description "Fractional graph theory, fuzzy graph theory"
    :parent 05Cxx)

(define-msc-2010-classification 05C75
    :name "05C75"
    :description "Structural characterization of families of graphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C76
    :name "05C76"
    :description "Graph operations (line graphs, products, etc.)"
    :parent 05Cxx)

(define-msc-2010-classification 05C78
    :name "05C78"
    :description "Graph labelling (graceful graphs, bandwidth, etc.)"
    :parent 05Cxx)

(define-msc-2010-classification 05C80
    :name "05C80"
    :description "Random graphs [See also 60B20]"
    :parent 05Cxx)

(define-msc-2010-classification 05C81
    :name "05C81"
    :description "Random walks on graphs"
    :parent 05Cxx)

(define-msc-2010-classification 05C82
    :name "05C82"
    :description "Small world graphs, complex networks"
    :see-also '(90Bxx 91D30)
    :parent 05Cxx)

(define-msc-2010-classification 05C83
    :name "05C83"
    :description "Graph minors"
    :parent 05Cxx)

(define-msc-2010-classification 05C85
    :name "05C85"
    :description "Graph algorithms"
    :see-also '(68R10 68W05)
    :parent 05Cxx)

(define-msc-2010-classification 05C90
    :name "05C90"
    :description "Applications"
    :see-also '(68R10 81Q30 81T15 82B20 82C20 90C35 92E10 94C15)
    :parent 05Cxx)

(define-msc-2010-classification 05C99
    :name "05C99"
    :description "None of the above, but in this section"
    :parent 05Cxx)

;; 05Dxx

(define-msc-2010-classification 05D05
    :name "05D05"
    :description "Extremal set theory"
    :parent 05Dxx) 

(define-msc-2010-classification 05D10
    :name "05D10"
    :description "Ramsey theory"
    :see-also '(05C55)
    :parent 05Dxx) 

(define-msc-2010-classification 05D15
    :name "05D15"
    :description "Transversal (matching) theory"
    :parent 05Dxx) 

(define-msc-2010-classification 05D40
    :name "05D40"
    :description "Probabilistic methods"
    :parent 05Dxx) 

(define-msc-2010-classification 05D99
    :name "05D99"
    :description "None of the above, but in this section"
    :parent 05Dxx) 

;; 05Exx

(define-msc-2010-classification 05E05
    :name "05E05"
    :description "Symmetric functions and generalizations"
    :parent 05Exx)

(define-msc-2010-classification 05E10
    :name "05E10"
    :description "Combinatorial aspects of representation theory"
    :see-also '(20C30)
    :parent 05Exx)

(define-msc-2010-classification 05E15
    :name "05E15"
    :description "Combinatorial aspects of groups and algebras"
    :see-also '(14Nxx 22E45 33C80)
    :parent 05Exx)

(define-msc-2010-classification 05E18
    :name "05E18"
    :description "Group actions on combinatorial structures"
    :parent 05Exx)

(define-msc-2010-classification 05E30
    :name "05E30"
    :description "Association schemes, strongly regular graphs"
    :parent 05Exx)

(define-msc-2010-classification 05E40
    :name "05E40"
    :description "Combinatorial aspects of commutative algebra"
    :parent 05Exx)

(define-msc-2010-classification 05E45
    :name "05E45"
    :description "Combinatorial aspects of simplicial complexes"
    :parent 05Exx)

(define-msc-2010-classification 05E99
    :name "05E99"
    :description "None of the above, but in this section"
    :parent 05Exx)

;; 06-XX

(define-msc-2010-classification 06-00
    :name "06-00"
    :description "General reference works (handbooks, dictionaries, bibliographies, etc.)"
    :parent 06-XX)

(define-msc-2010-classification 06-01
    :name "06-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 06-XX)

(define-msc-2010-classification 06-02
    :name "06-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 06-XX)

(define-msc-2010-classification 06-03
    :name "06-03"
    :description "Historical (must also be assigned at least one classification number from Section 01)"
    :parent 06-XX)

(define-msc-2010-classification 06-04
    :name "06-04"
    :description "Explicit machine computation and programs (not the theory of computation or programming)"
    :parent 06-XX)

(define-msc-2010-classification 06-06
    :name "06-06"
    :description "Proceedings, conferences, collections, etc."
    :parent 06-XX)

(define-msc-2010-classification 06Axx
    :name "06Axx"
    :description "Ordered sets"
    :parent 06-XX
    :contains (06A05 06A06 06A07 06A11 06A12 06A15 06A75 06A99))

(define-msc-2010-classification 06Bxx
    :name "06Bxx"
    :description "Lattices"
    :see-also '(03G10)
    :parent 06-XX
    :contains (06B05 06B10 06B15 06B20 06B23 06B25 06B30 06B35 06B75 06B99))

(define-msc-2010-classification 06Cxx
    :name "06Cxx"
    :description "Modular lattices, complemented lattices"
    :parent 06-XX
    :contains (06C05 06C10 06C15 06C20 06C99))

(define-msc-2010-classification 06Dxx
    :name "06Dxx"
    :description "Distributive lattices"
    :parent 06-XX
    :contains (06D05 06D10 06D15 06D20 06D22 06D25 06D30 06D35 06D50 06D72 06D75 06D99))

(define-msc-2010-classification 06Exx
    :name "06Exx"
    :description "Boolean algebras (Boolean rings)"
    :see-also '(03G05)
    :parent 06-XX
    :contains '(06E05 06E10 06E15 06E20 06E25 06E30 06E75 06E99))

(define-msc-2010-classification 06Fxx
    :name "06Fxx"
    :description "Ordered structures"
    :parent 06-XX
    :contains (06F05 06F07 06F10 06F15 06F20 06F25 06F30 06F35 06F99))

;; 06Axx

(define-msc-2010-classification 06A05
    :name "06A05"
    :description "Total order"
    :parent 06Axx)

(define-msc-2010-classification 06A06
    :name "06A06"
    :description "Partial order, general"
    :parent 06Axx)

(define-msc-2010-classification 06A07
    :name "06A07"
    :description "Combinatorics of partially ordered sets"
    :parent 06Axx)

(define-msc-2010-classification 06A11
    :name "06A11"
    :description "Algebraic aspects of posets"
    :parent 06Axx)

(define-msc-2010-classification 06A12
    :name "06A12"
    :description "Semilattices"
    :see-also '(20M10
		("Topological semilattices" 22A26))
    :parent 06Axx)

(define-msc-2010-classification 06A15
    :name "06A15"
    :description "Galois correspondences, closure operators"
    :parent 06Axx)

(define-msc-2010-classification 06A75
    :name "06A75"
    :description "Generalizations of ordered sets"
    :parent 06Axx)

(define-msc-2010-classification 06A99
    :name "06A99"
    :description "None of the above, but in this section"
    :parent 06Axx)

;; 06Bxx

(define-msc-2010-classification 06B05
    :name "06B05"
    :description "Structure theory"
    :parent 06Bxx)

(define-msc-2010-classification 06B10
    :name "06B10"
    :description "Ideals, congruence relations"
    :parent 06Bxx)

(define-msc-2010-classification 06B15
    :name "06B15"
    :description "Representation theory"
    :parent 06Bxx)

(define-msc-2010-classification 06B20
    :name "06B20"
    :description "Varieties of lattices"
    :parent 06Bxx)

(define-msc-2010-classification 06B23
    :name "06B23"
    :description "Complete lattices, completions"
    :parent 06Bxx)

(define-msc-2010-classification 06B25
    :name "06B25"
    :description "Free lattices, projective lattices, word problems"
    :see-also '(03D40 08A50 20F10)
    :parent 06Bxx)

(define-msc-2010-classification 06B30
    :name "06B30"
    :description "Topological lattices, order topologies"
    :see-also '(06F30 22A26 54F05 54H12)
    :parent 06Bxx)

(define-msc-2010-classification 06B35
    :name "06B35"
    :description "Continuous lattices and posets, applications"
    :see-also '(06B30 06D10 06F30 18B35 22A26 68Q55)
    :parent 06Bxx)

(define-msc-2010-classification 06B75
    :name "06B75"
    :description "Generalizations of lattices"
    :parent 06Bxx)

(define-msc-2010-classification 06B99
    :name "06B99"
    :description "None of the above, but in this section"
    :parent 06Bxx)

;; 06Cxx

(define-msc-2010-classification 06C05
    :name "06C05"
    :description "Modular lattices, Desarguesian lattices"
    :parent 06Cxx)

(define-msc-2010-classification 06C10
    :name "06C10"
    :description "Semimodular lattices, geometric lattices"
    :parent 06Cxx)

(define-msc-2010-classification 06C15
    :name "06C15"
    :description "Complemented lattices, orthocomplemented lattices and posets"
    :see-also '(03G12 81P10)
    :parent 06Cxx)

(define-msc-2010-classification 06C20
    :name "06C20"
    :description "Complemented modular lattices, continuous geometries"
    :parent 06Cxx)

(define-msc-2010-classification 06C99
    :name "06C99"
    :description "None of the above, but in this section"
    :parent 06Cxx)

;; 06Dxx

(define-msc-2010-classification 06D05
    :name "06D05"
    :description "Structure and representation theory"
    :parent 06Dxx)

(define-msc-2010-classification 06D10
    :name "06D10"
    :description "Complete distributivity"
    :parent 06Dxx)

(define-msc-2010-classification 06D15
    :name "06D15"
    :description "Pseudocomplemented lattices"
    :parent 06Dxx)

(define-msc-2010-classification 06D20
    :name "06D20"
    :description "Heyting algebras"
    :see-also '(03G25)
    :parent 06Dxx)

(define-msc-2010-classification 06D22
    :name "06D22"
    :description "Frames, locales"
    :see-also '(("Topological questions" 54-XX))
    :parent 06Dxx)

(define-msc-2010-classification 06D25
    :name "06D25"
    :description "Post algebras [See also 03G20]"
    :parent 06Dxx)

(define-msc-2010-classification 06D30
    :name "06D30"
    :description "De Morgan algebras, Łukasiewicz algebras"
    :see-also '(03G20)
    :parent 06Dxx)

(define-msc-2010-classification 06D35
    :name "06D35"
    :description "MV-algebras"
    :parent 06Dxx)

(define-msc-2010-classification 06D50
    :name "06D50"
    :description "Lattices and duality"
    :parent 06Dxx)

(define-msc-2010-classification 06D72
    :name "06D72"
    :description "Fuzzy lattices (soft algebras) and related topics"
    :parent 06Dxx)

(define-msc-2010-classification 06D75
    :name "06D75"
    :description "Other generalizations of distributive lattices"
    :parent 06Dxx)

(define-msc-2010-classification 06D99
    :name "06D99"
    :description "None of the above, but in this section"
    :parent 06Dxx)

;; 06Exx

(define-msc-2010-classification 06E05
    :name "06E05"
    :description "Structure theory"
    :parent 06Exx)

(define-msc-2010-classification 06E10
    :name "06E10"
    :description "Chain conditions, complete algebras"
    :parent 06Exx)

(define-msc-2010-classification 06E15
    :name "06E15"
    :description "Stone spaces (Boolean spaces) and related structures"
    :parent 06Exx)

(define-msc-2010-classification 06E20
    :name "06E20"
    :description "Ring-theoretic properties"
    :see-also '(16E50 16G30)
    :parent 06Exx)

(define-msc-2010-classification 06E25
    :name "06E25"
    :description "Boolean algebras with additional operations (diagonalizable algebras, etc.)"
    :see-also '(03G25 03F45)
    :parent 06Exx)

(define-msc-2010-classification 06E30
    :name "06E30"
    :description "Boolean functions"
    :see-also '(94C10)
    :parent 06Exx)

(define-msc-2010-classification 06E75
    :name "06E75"
    :description "Generalizations of Boolean algebras"
    :parent 06Exx)

(define-msc-2010-classification 06E99
    :name "06E99"
    :description "None of the above, but in this section"
    :parent 06Exx)

;; 06Fxx

(define-msc-2010-classification 06F05
    :name "06F05"
    :description "Ordered semigroups and monoids"
    :see-also '(20Mxx)
    :parent 06Fxx)

(define-msc-2010-classification 06F07
    :name "06F07"
    :description "Quantales"
    :parent 06Fxx)

(define-msc-2010-classification 06F10
    :name "06F10"
    :description "Noether lattices"
    :parent 06Fxx)

(define-msc-2010-classification 06F15
    :name "06F15"
    :description "Ordered groups"
    :see-also '(20F60)
    :parent 06Fxx)

(define-msc-2010-classification 06F20
    :name "06F20"
    :description "Ordered abelian groups, Riesz groups, ordered linear spaces"
    :see-also '(46A40)
    :parent 06Fxx)

(define-msc-2010-classification 06F25
    :name "06F25"
    :description "Ordered rings, algebras, modules"
    :see-also '(("Ordered fields" 12J15)
		13J25
		16W80)
    :parent 06Fxx)

(define-msc-2010-classification 06F30
    :name "06F30"
    :description "Topological lattices, order topologies"
    :see-also '(06B30 22A26 54F05 54H12)
    :parent 06Fxx)

(define-msc-2010-classification 06F35
    :name "06F35"
    :description "BCK-algebras, BCI-algebras"
    :see-also '([03G25)
    :parent 06Fxx)

(define-msc-2010-classification 06F99
    :name "06F99"
    :description "None of the above, but in this section"
    :parent 06Fxx)

;; 08-XX

(define-msc-2010-classification 08-00
    :name "08-00"
    :description "General reference works (handbooks, dictionaries, bibliographies, etc.)"
    :parent 08-XX)

(define-msc-2010-classification 08-01
    :name "08-01"
    :description "Instructional exposition (textbooks, tutorial papers, etc.)"
    :parent 08-XX)

(define-msc-2010-classification 08-02
    :name "08-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 08-XX)

(define-msc-2010-classification 08-03
    :name "08-03"
    :description "Historical (must also be assigned at least one classification number from Section 01)"
    :parent 08-XX)

(define-msc-2010-classification 08-04
    :name "08-04"
    :description "Explicit machine computation and programs (not the theory of computation or programming)"
    :parent 08-XX)

(define-msc-2010-classification 08-06
    :name "08-06"
    :description "Proceedings, conferences, collections, etc."
    :parent 08-XX)

(define-msc-2010-classification 08Axx
    :name "08Axx"
    :description "Algebraic structures"
    :see-also '(03C05)
    :parent 08-XX
    :contains (08A02 08A05 08A30 08A35 08A40 08A45 08A50 08A55 08A60 08A62 08A65 08A68 08A70 08A72 08A99))

(define-msc-2010-classification 08Bxx
    :name "08Bxx"
    :description "Varieties"
    :see-also '(03C05)
    :parent 08-XX
    :contains (08B05 08B10 08B15 08B20 08B25 08B26 08B30 08B99))

(define-msc-2010-classification 08Cxx
    :name "08Cxx"
    :description "Other classes of algebras"
    :parent 08-XX
    :contains (08C05 08C10 08C15 08C20 08C99))

;; 08Axx

(define-msc-2010-classification 08A02
    :name "08A02"
    :description "Relational systems, laws of composition"
    :parent 08Axx)

(define-msc-2010-classification 08A05
    :name "08A05"
    :description "Structure theory"
    :parent 08Axx)

(define-msc-2010-classification 08A30
    :name "08A30"
    :description "Subalgebras, congruence relations"
    :parent 08Axx)

(define-msc-2010-classification 08A35
    :name "08A35"
    :description "Automorphisms, endomorphisms"
    :parent 08Axx)

(define-msc-2010-classification 08A40
    :name "08A40"
    :description "Operations, polynomials, primal algebras"
    :parent 08Axx)

(define-msc-2010-classification 08A45
    :name "08A45"
    :description "Equational compactness"
    :parent 08Axx)

(define-msc-2010-classification 08A50
    :name "08A50"
    :description "Word problems"
    :see-also '(03D40 06B25 20F10 68R15)
    :parent 08Axx)

(define-msc-2010-classification 08A55
    :name "08A55"
    :description "Partial algebras"
    :parent 08Axx)

(define-msc-2010-classification 08A60
    :name "08A60"
    :description "Unary algebras"
    :parent 08Axx)

(define-msc-2010-classification 08A62
    :name "08A62"
    :description "Finitary algebras"
    :parent 08Axx)

(define-msc-2010-classification 08A65
    :name "08A65"
    :description "Infinitary algebras"
    :parent 08Axx)

(define-msc-2010-classification 08A68
    :name "08A68"
    :description "Heterogeneous algebras"
    :parent 08Axx)

(define-msc-2010-classification 08A70
    :name "08A70"
    :description "Applications of universal algebra in computer science"
    :parent 08Axx)

(define-msc-2010-classification 08A72
    :name "08A72"
    :description "Fuzzy algebraic structures"
    :parent 08Axx)

(define-msc-2010-classification 08A99
    :name "08A99"
    :description "None of the above, but in this section"
    :parent 08Axx)

;; 08Bxx

(define-msc-2010-classification 08B05
    :name "08B05"
    :description "Equational logic, Mal′cev (Mal′tsev) conditions"
    :parent 08Bxx)

(define-msc-2010-classification 08B10
    :name "08B10"
    :description "Congruence modularity, congruence distributivity"
    :parent 08Bxx)

(define-msc-2010-classification 08B15
    :name "08B15"
    :description "Lattices of varieties"
    :parent 08Bxx)

(define-msc-2010-classification 08B20
    :name "08B20"
    :description "Free algebras"
    :parent 08Bxx)

(define-msc-2010-classification 08B25
    :name "08B25"
    :description "Products, amalgamated products, and other kinds of limits and colimits"
    :see-also '(18A30)
    :parent 08Bxx)

(define-msc-2010-classification 08B26
    :name "08B26"
    :description "Subdirect products and subdirect irreducibility"
    :parent 08Bxx)

(define-msc-2010-classification 08B30
    :name "08B30"
    :description "Injectives, projectives"
    :parent 08Bxx)

(define-msc-2010-classification 08B99
    :name "08B99"
    :description "None of the above, but in this section"
    :parent 08Bxx)

;; 08Cxx

(define-msc-2010-classification 08C05
    :name "08C05"
    :description "Categories of algebras"
    :see-also '(18C05)
    :parent 08Cxx)

(define-msc-2010-classification 08C10
    :name "08C10"
    :description "Axiomatic model classes"
    :see-also '(03Cxx 03C60)
    :parent 08Cxx)

(define-msc-2010-classification 08C15
    :name "08C15"
    :description "Quasivarieties"
    :parent 08Cxx)

(define-msc-2010-classification 08C20
    :name "08C20"
    :description "Natural dualities for classes of algebras"
    :see-also '(06E15 18A40 22A30)
    :parent 08Cxx)

(define-msc-2010-classification 08C99
    :name "08C99"
    :description "None of the above, but in this section"
    :parent 08Cxx)

;; 11-XX

(define-msc-2010-classification 11-00
    :name "11-00"
    :description "General reference works (handbooks, dictionaries, bibliographies, etc.)"
    :parent 11-XX)

(define-msc-2010-classification 11-01
    :name "11-01"
    :description ) Instructional "xposition (textbooks, tutorial papers, etc.)"
:parent 11-XX

(define-msc-2010-classification 11-02
    :name "11-02"
    :description "Research exposition (monographs, survey articles)"
    :parent 11-XX)

(define-msc-2010-classification 11-03
    :name "11-03"
    :description "Historical (must also be assigned at least one classification number from Section 01)"
    :parent 11-XX)

(define-msc-2010-classification 11-04
    :name "11-04"
    :description "Explicit machine computation and programs (not the theory of computation or programming)"
    :parent 11-XX)

(define-msc-2010-classification 11-06
    :name "11-06"
    :description "Proceedings, conferences, collections, etc."
    :parent 11-XX)

(define-msc-2010-classification 11Axx
    :name "11Axx"
    :description "Elementary number theory {For analogues in number fields, see 11R04}"
    :parent 11-XX
    :contains '(11A05 11A07 11A15 11A25 11A41 11A51 11A55 11A63 11A67 11A99))

(define-msc-2010-classification 11Bxx
    :name "11Bxx"
    :description "Sequences and sets"
    :parent 11-XX
    :contains '(11B05 11B13 11B25 11B30 11B34 11B37 11B39 11B50 11B57 11B65 11B68 11B73 11B75 11B83 11B85 11B99))

(define-msc-2010-classification 11Cxx
    :name "11Cxx"
    :description "Polynomials and matrices"
    :parent 11-XX
    :contains '(11C08 11C20 11C99))

(define-msc-2010-classification 11Dxx
    :name "11Dxx"
    :description "Diophantine equations"
    :see-also '(11Gxx 14Gxx)
    :parent 11-XX
    :contains '(11D04 11D09 11D25 11D41 11D45 11D57 11D59 11D61 11D68 11D72 11D75 11D79 11D85 11D88 11D99))

(define-msc-2010-classification 11Exx
    :name "11Exx"
    :description "Forms and linear algebraic groups"
    :see-also '(19Gxx
		("Quadratic forms in linear algebra" 15A63))
    :parent 11-XX
    :contains (11E04 11E08 11E10 11E12 11E16 11E20 11E25 11E39 11E41 11E45 11E57 11E70 11E72 11E76 11E81 11E88 11E95 11E99))

(define-msc-2010-classification 11Fxx
    :name "11Fxx"
    :description "Discontinuous groups and automorphic forms"
    :see-also '(11R39 11S37 14Gxx 14Kxx 22E50 22E55 30F35 32Nxx
		("Relations with quadratic forms" 11E45}))
    :parent 11-XX
    :contains (11F03 11F06 11F11 11F12 11F20 11F22 11F23 11F25 11F27 11F30 11F32 11F33 11F37 11F41 11F46 11F50 11F52 11F55 11F60 11F66 11F67 11F68 11F70 11F72 11F75 11F80 11F85 11F99))

(define-msc-2010-classification 11Gxx
    :name "11Gxx"
    :description "Arithmetic algebraic geometry (Diophantine geometry)"
    :see-also '(11Dxx 14Gxx 14Kxx)
    :parent 11-XX
    :contains '(11G05 11G07 11G09 11G10 11G15 11G16 11G18 11G20 11G25 11G30 11G32 11G35 11G40 11G42 11G45 11G50 11G55 11G99))

(define-msc-2010-classification 11Hxx
    :name "11Hxx"
    :description "Geometry of numbers"
    :see-also '(("Applications in coding theory" 94B75))
    :parent 11-XX
    :contains '(11H06 11H16 11H31 11H46 11H50 11H55 11H56 11H60 11H71 11H99))

(define-msc-2010-classification 11Jxx
    :name "11Jxx"
    :description "Diophantine approximation, transcendental number theory"
    :see-also '(11K60)
    :parent 11-XX
    :contains '(11J04 11J06 11J13 11J17 11J20 11J25 11J54 11J61 11J68 11J70 11J71 11J72 11J81 11J82 11J83 11J85 11J86 11J87 11J89 11J91 11J93 11J95 11J97 11J99))

(define-msc-2010-classification 11Kxx
    :name "11Kxx"
    :description "Probabilistic theory: distribution modulo $1$; metric theory of algorithms"
    :parent 11-XX
    :contains '(11K06 11K16 11K31 11K36 11K38 11K41 11K45 11K50 11K55 11K60 11K65 11K70 11K99))

(define-msc-2010-classification 11Lxx
    :name "11Lxx"
    :description "Exponential sums and character sums"
    :see-also '(("Finite fields" 11Txx))
    :parent 11-XX
    :contains '(11L03 11L05 11L07 11L10 11L15 11L20 11L26 11L40 11L99))

(define-msc-2010-classification 11Mxx
    :name "11Mxx"
    :description "Zeta and $L$-functions: analytic theory"
    :parent 11-XX
    :contains (11M06 11M20 11M26 11M32 11M35 11M36 11M38 11M41 11M45 11M50 11M55 11M99))

(define-msc-2010-classification 11Nxx
    :name "11Nxx"
    :description "Multiplicative number theory"
    :parent 11-XX
    :contains (11N05 11N13 11N25 11N30 11N32 11N35 11N36 11N37 11N45 11N56 11N60 11N64 11N69 11N75 11N80 11N99))

(define-msc-2010-classification 11Pxx
    :name "11Pxx"
    :description "Additive number theory; partitions"
    :parent 11-XX
    :contains '(11P05 11P21 11P32 11P55 11P70 11P81 11P82 11P83 11P84 11P99))

(define-msc-2010-classification 11Rxx
    :name "11Rxx"
    :description "Algebraic number theory: global fields"
    :see-also '(("Complex multiplication" 11G15))
    :parent 11-XX
    :contains '(11R04 11R06 11R09 11R11 11R16 11R18 11R20 11R21 11R23 11R27 11R29 11R32 11R33 11R34 11R37 11R39 11R42 11R44 11R45 11R47 11R52 11R54 11R56 11R58 11R60 11R65 11R70 11R80 11R99))

(define-msc-2010-classification 11Sxx
    :name "11Sxx"
    :description "Algebraic number theory: local and $p$-adic fields"
    :parent 11-XX
    :contains '(11S05 11S15 11S20 11S23 11S25 11S31 11S37 11S40 11S45 11S70 11S80 11S82 11S85 11S90 11S99))

(define-msc-2010-classification 11Txx
    :name "11Txx"
    :description "Finite fields and commutative rings (number-theoretic aspects)"
    :parent 11-XX
    :contains '(11T06 11T22 11T23 11T24 11T30 11T55 11T60 11T71 11T99))  

(define-msc-2010-classification 11Uxx
    :name "11Uxx"
    :description "Connections with logic"
    :parent 11-XX
    :contains '(11U05 11U07 11U09 11U10 11U99))

(define-msc-2010-classification 11Yxx
    :name "11Yxx"
    :description "Computational number theory"
    :see-also '(11-04)
    :parent 11-XX
    :contains '(11Y05 11Y11 11Y16 11Y35 11Y40 11Y50 11Y55 11Y60 11Y65 11Y70 11Y99))

(define-msc-2010-classification 11Zxx
    :name "11Zxx"
    :description "Miscellaneous applications of number theory"
    :parent 11-XX
    :contains (11Z05 11Z99))

;; 11Axx

(define-msc-2010-classification 11A05
    :name "11A05"
    :description "Multiplicative structure; Euclidean algorithm; greatest common divisors"
    :parent 11Axx)

(define-msc-2010-classification 11A07
    :name "11A07"
    :description "Congruences; primitive roots; residue systems"
    :parent 11Axx)

(define-msc-2010-classification 11A15
    :name "11A15"
    :description "Power residues, reciprocity"
    :parent 11Axx)

(define-msc-2010-classification 11A25
    :name "11A25"
    :description "Arithmetic functions; related numbers; inversion formulas"
    :parent 11Axx)

(define-msc-2010-classification 11A41
    :name "11A41"
    :description "Primes"
    :parent 11Axx)

(define-msc-2010-classification 11A51
    :name "11A51"
    :description "Factorization; primality"
    :parent 11Axx)

(define-msc-2010-classification 11A55
    :name "11A55"
    :description "Continued fractions"
    :see-also '(11K50 30B70 40A15
		("Approximation results" 11J70))
    :parent 11Axx)

(define-msc-2010-classification 11A63
    :name "11A63"
    :description "Radix representation; digital problems"
    :see-also '(("Metric results" 11K16))
    :parent 11Axx)

(define-msc-2010-classification 11A67
    :name "11A67"
    :description "Other representations"
    :parent 11Axx)

(define-msc-2010-classification 11A99
    :name "11A99"
    :description "None of the above, but in this section"
    :parent 11Axx)

;; 11Bxx

(define-msc-2010-classification 11B05
    :name "11B05"
    :description "Density, gaps, topology"
    :parent 11Bxx)

(define-msc-2010-classification 11B13
    :name "11B13"
    :description "Additive bases, including sumsets"
    :see-also '(05B10)
    :parent 11Bxx)

(define-msc-2010-classification 11B25
    :name "11B25"
    :description "Arithmetic progressions"
    :see-also '(11N13)
    :parent 11Bxx)

(define-msc-2010-classification 11B30
    :name "11B30"
    :description "Arithmetic combinatorics; higher degree uniformity"
    :parent 11Bxx)

(define-msc-2010-classification 11B34
    :name "11B34"
    :description "Representation functions"
    :parent 11Bxx)

(define-msc-2010-classification 11B37
    :name "11B37"
    :description "Recurrences"
    :see-also (("Applications to special functions" 33-XX))
    :parent 11Bxx)

(define-msc-2010-classification 11B39
    :name "11B39"
    :description "Fibonacci and Lucas numbers and polynomials and generalizations"
    :parent 11Bxx)

(define-msc-2010-classification 11B50
    :name "11B50"
    :description "Sequences (mod $m$)"
    :parent 11Bxx)

(define-msc-2010-classification 11B57
    :name "11B57"
    :description "Farey sequences; the sequences ${1^k, 2^k, \cdots}$"
    :parent 11Bxx)

(define-msc-2010-classification 11B65
    :name "11B65"
    :description "Binomial coefficients; factorials; $q$-identities"
    :see-also '(05A10 05A30)
    :parent 11Bxx)

(define-msc-2010-classification 11B68
    :name "11B68"
    :description "Bernoulli and Euler numbers and polynomials"
    :parent 11Bxx)

(define-msc-2010-classification 11B73
    :name "11B73"
    :description "Bell and Stirling numbers"
    :parent 11Bxx)

(define-msc-2010-classification 11B75
    :name "11B75"
    :description "Other combinatorial number theory"
    :parent 11Bxx)

(define-msc-2010-classification 11B83
    :name "11B83"
    :description "Special sequences and polynomials"
    :parent 11Bxx)

(define-msc-2010-classification 11B85
    :name "11B85"
    :description "Automata sequences"
    :parent 11Bxx)

(define-msc-2010-classification 11B99
    :name "11B99"
    :description "None of the above, but in this section"
    :parent 11Bxx)

;; 11Cxx

(define-msc-2010-classification 11C08
    :name "11C08"
    :description "Polynomials"
    :see-also '(13F20)
    :parent 11xx)

(define-msc-2010-classification 11C20
    :name "11C20"
    :description "Matrices, determinants"
    :parent 11xx)

(define-msc-2010-classification 11C99
    :name "11C99"
    :description "None of the above, but in this section"
    :parent 11xx)

;; 11Dxx

(define-msc-2010-classification 11D04
    :name "11D04"
    :description "Linear equations"
    :parent 11Dxx)

(define-msc-2010-classification 11D07
    :name "11D07"
    :description "The Frobenius problem"
    :parent 11Dxx)

(define-msc-2010-classification 11D09
    :name "11D09"
    :description "Quadratic and bilinear equations"
    :parent 11Dxx)

(define-msc-2010-classification 11D25
    :name "11D25"
    :description "Cubic and quartic equations"
    :parent 11Dxx)

(define-msc-2010-classification 11D41
    :name "11D41"
    :description "Higher degree equations; Fermat's equation"
    :parent 11Dxx)

(define-msc-2010-classification 11D45
    :name "11D45"
    :description "Counting solutions of Diophantine equations"
    :parent 11Dxx)

(define-msc-2010-classification 11D57
    :name "11D57"
    :description "Multiplicative and norm form equations"
    :parent 11Dxx)

(define-msc-2010-classification 11D59
    :name "11D59"
    :description "Thue-Mahler equations"
    :parent 11Dxx)

(define-msc-2010-classification 11D61
    :name "11D61"
    :description "Exponential equations"
    :parent 11Dxx)

(define-msc-2010-classification 11D68
    :name "11D68"
    :description "Rational numbers as sums of fractions"
    :parent 11Dxx)

(define-msc-2010-classification 11D72
    :name "11D72"
    :description "Equations in many variables"
    :see-also '(11P55)
    :parent 11Dxx)

(define-msc-2010-classification 11D75
    :name "11D75"
    :description "Diophantine inequalities"
    :see-also '(11J25)
    :parent 11Dxx)

(define-msc-2010-classification 11D79
    :name "11D79"
    :description "Congruences in many variables"
    :parent 11Dxx)

(define-msc-2010-classification 11D85
    :name "11D85"
    :description "Representation problems"
    :see-also '(11P55)
    :parent 11Dxx)

(define-msc-2010-classification 11D88
    :name "11D88"
    :description "$p$-adic and power series fields"
    :parent 11Dxx)

(define-msc-2010-classification 11D99
    :name "11D99"
    :description "None of the above, but in this section"
    :parent 11Dxx)

;; 11Exx

(define-msc-2010-classification 11E04
    :name "11E04"
    :description "Quadratic forms over general fields"
    :parent 11Exx)

(define-msc-2010-classification 11E08
    :name "11E08"
    :description "Quadratic forms over local rings and fields"
    :parent 11Exx)

(define-msc-2010-classification 11E10
    :name "11E10"
    :description "Forms over real fields"
    :parent 11Exx)

(define-msc-2010-classification 11E12
    :name "11E12"
    :description "Quadratic forms over global rings and fields"
    :parent 11Exx)

(define-msc-2010-classification 11E16
    :name "11E16"
    :description "General binary quadratic forms"
    :parent 11Exx)

(define-msc-2010-classification 11E20
    :name "11E20"
    :description "General ternary and quaternary quadratic forms; forms of more than two variables"
    :parent 11Exx)

(define-msc-2010-classification 11E25
    :name "11E25"
    :description "Sums of squares and representations by other particular quadratic forms"
    :parent 11Exx)

(define-msc-2010-classification 11E39
    :name "11E39"
    :description "Bilinear and Hermitian forms"
    :parent 11Exx)

(define-msc-2010-classification 11E41
    :name "11E41"
    :description "Class numbers of quadratic and Hermitian forms"
    :parent 11Exx)

(define-msc-2010-classification 11E45
    :name "11E45"
    :description "Analytic theory (Epstein zeta functions; relations with automorphic forms and functions)"
    :parent 11Exx)

(define-msc-2010-classification 11E57
    :name "11E57"
    :description "Classical groups"
    :see-also '(14Lxx 20Gxx)
    :parent 11Exx)

(define-msc-2010-classification 11E70
    :name "11E70"
    :description "$K$-theory of quadratic and Hermitian forms"
    :parent 11Exx)

(define-msc-2010-classification 11E72
    :name "11E72"
    :description "Galois cohomology of linear algebraic groups"
    :see-also '(20G10)
    :parent 11Exx)

(define-msc-2010-classification 11E76
    :name "11E76"
    :description "Forms of degree higher than two"
    :parent 11Exx)

(define-msc-2010-classification 11E81
    :name "11E81"
    :description "Algebraic theory of quadratic forms; Witt groups and rings"
    :see-also '(19G12 19G24)
    :parent 11Exx)

(define-msc-2010-classification 11E88
    :name "11E88"
    :description "Quadratic spaces; Clifford algebras"
    :see-also '(15A63 15A66)
    :parent 11Exx)

(define-msc-2010-classification 11E95
    :name "11E95"
    :description "$p$-adic theory"
    :parent 11Exx)

(define-msc-2010-classification 11E99
    :name "11E99"
    :description "None of the above, but in this section"
    :parent 11Exx)

;; 11Fxx

(define-msc-2010-classification 11F03
    :name "11F03"
    :description "Modular and automorphic functions"
    :parent 11Fxx)

(define-msc-2010-classification 11F06
    :name "11F06"
    :description "Structure of modular groups and generalizations; arithmetic groups"
    :see-also '(20H05 20H10 22E40)
    :parent 11Fxx)

(define-msc-2010-classification 11F11
    :name "11F11"
    :description "Holomorphic modular forms of integral weight"
    :parent 11Fxx)

(define-msc-2010-classification 11F12
    :name "11F12"
    :description "Automorphic forms, one variable"
    :parent 11Fxx)

(define-msc-2010-classification 11F20
    :name "11F20"
    :description "Dedekind eta function, Dedekind sums"
    :parent 11Fxx)

(define-msc-2010-classification 11F22
    :name "11F22"
    :description "Relationship to Lie algebras and finite simple groups"
    :parent 11Fxx)

(define-msc-2010-classification 11F23
    :name "11F23"
    :description "Relations with algebraic geometry and topology"
    :parent 11Fxx)

(define-msc-2010-classification 11F25
    :name "11F25"
    :description "Hecke-Petersson operators, differential operators (one variable)"
    :parent 11Fxx)

(define-msc-2010-classification 11F27
    :name "11F27"
    :description "Theta series; Weil representation; theta correspondences"
    :parent 11Fxx)

(define-msc-2010-classification 11F30
    :name "11F30"
    :description "Fourier coefficients of automorphic forms"
    :parent 11Fxx)

(define-msc-2010-classification 11F32
    :name "11F32"
    :description "Modular correspondences, etc."
    :parent 11Fxx)

(define-msc-2010-classification 11F33
    :name "11F33"
    :description "Congruences for modular and $p$-adic modular forms"
    :see-also '(14G20 22E50)
    :parent 11Fxx)

(define-msc-2010-classification 11F37
    :name "11F37"
    :description "Forms of half-integer weight; nonholomorphic modular forms"
    :parent 11Fxx)

(define-msc-2010-classification 11F41
    :name "11F41"
    :description "Automorphic forms on ${\rm GL}(2)$; Hilbert and Hilbert-Siegel modular groups and their modular and automorphic forms; Hilbert modular surfaces"
    :see-also '(14J20)
    :parent 11Fxx)

(define-msc-2010-classification 11F46
    :name "11F46"
    :description "Siegel modular groups; Siegel and Hilbert-Siegel modular and automorphic forms"
    :parent 11Fxx)

(define-msc-2010-classification 11F50
    :name "11F50"
    :description "Jacobi forms"
    :parent 11Fxx)

(define-msc-2010-classification 11F52
    :name "11F52"
    :description "Modular forms associated to Drinfel'd modules"
    :parent 11Fxx)

(define-msc-2010-classification 11F55
    :name "11F55"
    :description "Other groups and their modular and automorphic forms (several variables)"
    :parent 11Fxx)

(define-msc-2010-classification 11F60
    :name "11F60"
    :description "Hecke-Petersson operators, differential operators (several variables)"
    :parent 11Fxx)

(define-msc-2010-classification 11F66
    :name "11F66"
    :description "Langlands $L$-functions; one variable Dirichlet series and functional equations"
    :parent 11Fxx)

(define-msc-2010-classification 11F67
    :name "11F67"
    :description "Special values of automorphic $L$-series, periods of modular forms, cohomology, modular symbols"
    :parent 11Fxx)

(define-msc-2010-classification 11F68
    :name "11F68"
    :description "Dirichlet series in several complex variables associated to automorphic forms; Weyl group multiple Dirichlet series"
    :parent 11Fxx)

(define-msc-2010-classification 11F70
    :name "11F70"
    :description "Representation-theoretic methods; automorphic representations over local and global fields"
    :parent 11Fxx)

(define-msc-2010-classification 11F72
    :name "11F72"
    :description "Spectral theory; Selberg trace formula"
    :parent 11Fxx)

(define-msc-2010-classification 11F75
    :name "11F75"
    :description "Cohomology of arithmetic groups"
    :parent 11Fxx)

(define-msc-2010-classification 11F80
    :name "11F80"
    :description "Galois representations"
    :parent 11Fxx)

(define-msc-2010-classification 11F85
    :name "11F85"
    :description "$p$-adic theory, local fields"
    :see-also '(14G20 22E50)
    :parent 11Fxx)

(define-msc-2010-classification 11F99
    :name "11F99"
    :description "None of the above, but in this section"
    :parent 11Fxx)

;; 11Gxx

(define-msc-2010-classification 11G05
  :description "Elliptic curves over global fields"
  :see-also '(14H52)
  :parent 11Gxx)

(define-msc-2010-classification 11G07
  :description "Elliptic curves over local fields"
  :see-also '(14G20 14H52)
  :parent 11Gxx)

(define-msc-2010-classification 11G09
  :description "Drinfel&apos;d modules; higher-dimensional motives, etc."
  :see-also '(14L05)
  :parent 11Gxx)

(define-msc-2010-classification 11G10
  :description "Abelian varieties of dimension $> 1$"
  :see-also '(14Kxx)
  :parent 11Gxx)

(define-msc-2010-classification 11G15
  :description "Complex multiplication and moduli of abelian varieties"
  :see-also '(14K22)
  :parent 11Gxx)

(define-msc-2010-classification 11G16
  :description "Elliptic and modular units"
  :see-also '(11R27)
  :parent 11Gxx)

(define-msc-2010-classification 11G18
  :description "Arithmetic aspects of modular and Shimura varieties"
  :see-also '(14G35)
  :parent 11Gxx)

(define-msc-2010-classification 11G20
  :description "Curves over finite and local fields"
  :see-also '(14H25)
  :parent 11Gxx)

(define-msc-2010-classification 11G25
  :description "Varieties over finite and local fields"
  :see-also '(14G15 14G20)
  :parent 11Gxx)

(define-msc-2010-classification 11G30
  :description "Curves of arbitrary genus or genus $\ne 1$ over global fields"
  :see-also '(14H25)
  :parent 11Gxx)

(define-msc-2010-classification 11G32
  :description "Dessins d'enfants, Belyĭ theory"
  :parent 11Gxx)

(define-msc-2010-classification 11G35
  :description "Varieties over global fields"
  :see-also '(14G25)
  :parent 11Gxx)

(define-msc-2010-classification 11G40
  :description "$L$-functions of varieties over global fields; Birch-Swinnerton-Dyer conjecture"
  :see-also '(14G10)
  :parent 11Gxx)

(define-msc-2010-classification 11G42
  :description "Arithmetic mirror symmetry"
  :see-also '(14J33)
  :parent 11Gxx)

(define-msc-2010-classification 11G45
  :description "Geometric class field theory"
  :see-also '(11R37 14C35 19F05)
  :parent 11Gxx)

(define-msc-2010-classification 11G50
  :description "Heights"
  :see-also '(14G40 37P30)
  :parent 11Gxx)

(define-msc-2010-classification 11G55
  :description "Polylogarithms and relations with $K$-theory"
  :parent 11Gxx)

(define-msc-2010-classification 11G99
  :description "None of the above, but in this section"
  :parent 11Gxx)

;; 11Hxx

(define-msc-2010-classification 11H06
  :description "Lattices and convex bodies"
  :see-also '(11P21 52C05 52C07)
  :parent 11Hxx)

(define-msc-2010-classification 11H16
  :description "Nonconvex bodies"
  :parent 11Hxx)

(define-msc-2010-classification 11H31
  :description "Lattice packing and covering"
  :see-also '(05B40 52C15 52C17)
  :parent 11Hxx)

(define-msc-2010-classification 11H46
  :description "Products of linear forms"
  :parent 11Hxx)

(define-msc-2010-classification 11H50
  :description "Minima of forms"
  :parent 11Hxx)

(define-msc-2010-classification 11H55
  :description "Quadratic forms (reduction theory, extreme forms, etc.)"
  :parent 11Hxx)

(define-msc-2010-classification 11H56
  :description "Automorphism groups of lattices"
  :parent 11Hxx)

(define-msc-2010-classification 11H60
  :description "Mean value and transfer theorems"
  :parent 11Hxx)

(define-msc-2010-classification 11H71
  :description "Relations with coding theory"
  :parent 11Hxx)

(define-msc-2010-classification 11H99
  :description "None of the above, but in this section"
  :parent 11Hxx)

;; 11Jxx

(define-msc-2010-classification 11J04
  :description "Homogeneous approximation to one number"
  :parent 11Jxx)

(define-msc-2010-classification 11J06
  :description "Markov and Lagrange spectra and generalizations"
  :parent 11Jxx)

(define-msc-2010-classification 11J13
  :description "Simultaneous homogeneous approximation, linear forms"
  :parent 11Jxx)

(define-msc-2010-classification 11J17
  :description "Approximation by numbers from a fixed field"
  :parent 11Jxx)

(define-msc-2010-classification 11J20
  :description "Inhomogeneous linear forms"
  :parent 11Jxx)

(define-msc-2010-classification 11J25
  :description "Diophantine inequalities"
  :see-also '(11D75)
  :parent 11Jxx)

(define-msc-2010-classification 11J54
  :description "Small fractional parts of polynomials and generalizations"
  :parent 11Jxx)

(define-msc-2010-classification 11J61
  :description "Approximation in non-Archimedean valuations"
  :parent 11Jxx)

(define-msc-2010-classification 11J68
  :description "Approximation to algebraic numbers"
  :parent 11Jxx)

(define-msc-2010-classification 11J70
  :description "Continued fractions and generalizations"
  :see-also '(11A55 11K50)
  :parent 11Jxx)

(define-msc-2010-classification 11J71
  :description "Distribution modulo one"
  :see-also '(11K06)
  :parent 11Jxx)

(define-msc-2010-classification 11J72
  :description "Irrationality; linear independence over a field"
  :parent 11Jxx)

(define-msc-2010-classification 11J81
  :description "Transcendence (general theory)"
  :parent 11Jxx)

(define-msc-2010-classification 11J82
  :description "Measures of irrationality and of transcendence"
  :parent 11Jxx)

(define-msc-2010-classification 11J83
  :description "Metric theory"
  :parent 11Jxx)

(define-msc-2010-classification 11J85
  :description "Algebraic independence; Gel'fond's method"
  :parent 11Jxx)

(define-msc-2010-classification 11J86
  :description "Linear forms in logarithms; Baker's method"
  :parent 11Jxx)

(define-msc-2010-classification 11J87
  :description "Schmidt Subspace Theorem and applications"
  :parent 11Jxx)

(define-msc-2010-classification 11J89
  :description "Transcendence theory of elliptic and abelian functions"
  :parent 11Jxx)

(define-msc-2010-classification 11J91
  :description "Transcendence theory of other special functions"
  :parent 11Jxx)

(define-msc-2010-classification 11J93
  :description "Transcendence theory of Drinfel'd and $t$-modules"
  :parent 11Jxx)

(define-msc-2010-classification 11J95
  :description "Results involving abelian varieties"
  :parent 11Jxx)

(define-msc-2010-classification 11J97
  :description "Analogues of methods in Nevanlinna theory (work of Vojta et al.)"
  :parent 11Jxx)

(define-msc-2010-classification 11J99
  :description "None of the above, but in this section"
  :parent 11Jxx)

;; 11Kxx

(define-msc-2010-classification 11K06
  :description "General theory of distribution modulo $1$"
  :see-also '(11J71)
  :parent 11Kxx)

(define-msc-2010-classification 11K16
  :description "Normal numbers, radix expansions, Pisot numbers, Salem numbers, good lattice points, etc."
  :see-also '(11A63)
  :parent 11Kxx)

(define-msc-2010-classification 11K31
  :description "Special sequences"
  :parent 11Kxx)

(define-msc-2010-classification 11K36
  :description "Well-distributed sequences and other variations"
  :parent 11Kxx)

(define-msc-2010-classification 11K38
  :description "Irregularities of distribution, discrepancy"
  :see-also '(11Nxx)
  :parent 11Kxx)

(define-msc-2010-classification 11K41
  :description "Continuous, $p$-adic and abstract analogues"
  :parent 11Kxx)

(define-msc-2010-classification 11K45
  :description "Pseudo-random numbers; Monte Carlo methods"
  :parent 11Kxx)

(define-msc-2010-classification 11K50
  :description "Metric theory of continued fractions"
  :see-also '(11A55 11J70)
  :parent 11Kxx)

(define-msc-2010-classification 11K55
  :description "Metric theory of other algorithms and expansions; measure and Hausdorff dimension"
  :see-also '(11N99 28Dxx)
  :parent 11Kxx)

(define-msc-2010-classification 11K60
  :description "Diophantine approximation"
  :see-also '(11Jxx)
  :parent 11Kxx)

(define-msc-2010-classification 11K65
  :description "Arithmetic functions"
  :see-also '(11Nxx)
  :parent 11Kxx)

(define-msc-2010-classification 11K70
  :description "Harmonic analysis and almost periodicity"
  :parent 11Kxx)

(define-msc-2010-classification 11K99
  :description "None of the above, but in this section"
  :parent 11Kxx)

;; 11Lxx

(define-msc-2010-classification 11L03
  :description "Trigonometric and exponential sums, general"
  :parent 11Lxx)

(define-msc-2010-classification 11L05
  :description "Gauss and Kloosterman sums; generalizations"
  :parent 11Lxx)

(define-msc-2010-classification 11L07
  :description "Estimates on exponential sums"
  :parent 11Lxx)

(define-msc-2010-classification 11L10
  :description "Jacobsthal and Brewer sums; other complete character sums"
  :parent 11Lxx)

(define-msc-2010-classification 11L15
  :description "Weyl sums"
  :parent 11Lxx)

(define-msc-2010-classification 11L20
  :description "Sums over primes"
  :parent 11Lxx)

(define-msc-2010-classification 11L26
  :description "Sums over arbitrary intervals"
  :parent 11Lxx)

(define-msc-2010-classification 11L40
  :description "Estimates on character sums"
  :parent 11Lxx)

(define-msc-2010-classification 11L99
  :description "None of the above, but in this section"
  :parent 11Lxx)

;; 11Mxx

(define-msc-2010-classification 11M06
  :description "$\zeta (s)$ and $L(s, \chi)$"
  :parent 11Mxx)

(define-msc-2010-classification 11M20
  :description "Real zeros of $L(s, \chi)$; results on $L(1, \chi)$"
  :parent 11Mxx)

(define-msc-2010-classification 11M26
  :description "Nonreal zeros of $\zeta (s)$ and $L(s, \chi)$; Riemann and other hypotheses"
  :parent 11Mxx)

(define-msc-2010-classification 11M32
  :description "Multiple Dirichlet series and zeta functions and multizeta values"
  :parent 11Mxx)

(define-msc-2010-classification 11M35
  :description "Hurwitz and Lerch zeta functions"
  :parent 11Mxx)

(define-msc-2010-classification 11M36
  :description "Selberg zeta functions and regularized determinants; applications to spectral theory, Dirichlet series, Eisenstein series, etc. Explicit formulas"
  :parent 11Mxx)

(define-msc-2010-classification 11M38
  :description "Zeta and $L$-functions in characteristic $p$"
  :parent 11Mxx)

(define-msc-2010-classification 11M41
  :description "Other Dirichlet series and zeta functions"
  :see-also '(("Local and global ground fields" 11R42 11R52 11S40 11S45)
	      ("Algebro-geometric methods" 14G10)
	      11E45 11F66 11F70 11F72)
  :parent 11Mxx)

(define-msc-2010-classification 11M45
  :description "Tauberian theorems"
  :see-also '(40E05)
  :parent 11Mxx)

(define-msc-2010-classification 11M50
  :description "Relations with random matrices"
  :parent 11Mxx)

(define-msc-2010-classification 11M55
  :description "Relations with noncommutative geometry"
  :parent 11Mxx)

(define-msc-2010-classification 11M99
  :description "None of the above, but in this section"
  :parent 11Mxx)

;; 11Nxx

(define-msc-2010-classification 11N05
  :description "Distribution of primes"
  :parent 11Nxx)

(define-msc-2010-classification 11N13
  :description "Primes in progressions"
  :contains '(11B25)
  :parent 11Nxx)

(define-msc-2010-classification 11N25
  :description "Distribution of integers with specified multiplicative constraints"
  :parent 11Nxx)

(define-msc-2010-classification 11N30
  :description "Turán theory"
  :see-also '(30Bxx)
  :parent 11Nxx)

(define-msc-2010-classification 11N32
  :description "Primes represented by polynomials; other multiplicative structure of polynomial values"
  :parent 11Nxx)

(define-msc-2010-classification 11N35
  :description "Sieves"
  :parent 11Nxx)

(define-msc-2010-classification 11N36
  :description "Applications of sieve methods"
  :parent 11Nxx)

(define-msc-2010-classification 11N37
  :description "Asymptotic results on arithmetic functions"
  :parent 11Nxx)

(define-msc-2010-classification 11N45
  :description "Asymptotic results on counting functions for algebraic and topological structures"
  :parent 11Nxx)

(define-msc-2010-classification 11N56
  :description "Rate of growth of arithmetic functions"
  :parent 11Nxx)

(define-msc-2010-classification 11N60
  :description "Distribution functions associated with additive and positive multiplicative functions"
  :parent 11Nxx)

(define-msc-2010-classification 11N64
  :description "Other results on the distribution of values or the characterization of arithmetic functions"
  :parent 11Nxx)

(define-msc-2010-classification 11N69
  :description "Distribution of integers in special residue classes"
  :parent 11Nxx)

(define-msc-2010-classification 11N75
  :description "Applications of automorphic functions and forms to multiplicative problems"
  :see-also '(11Fxx)
  :parent 11Nxx)

(define-msc-2010-classification 11N80
  :description "Generalized primes and integers"
  :parent 11Nxx)

(define-msc-2010-classification 11N99
  :description "None of the above, but in this section"
  :parent 11Nxx)

;; 11Pxx

(define-msc-2010-classification 11P05
  :description "Waring's problem and variants"
  :parent 11Pxx)

(define-msc-2010-classification 11P21
  :description "Lattice points in specified regions"
  :parent 11Pxx)

(define-msc-2010-classification 11P32
  :description "Goldbach-type theorems; other additive questions involving primes"
  :parent 11Pxx)

(define-msc-2010-classification 11P55
  :description "Applications of the Hardy-Littlewood method"
  :see-also '(11D85)
  :parent 11Pxx)

(define-msc-2010-classification 11P70
  :description "Inverse problems of additive number theory, including sumsets"
  :parent 11Pxx)

(define-msc-2010-classification 11P81
  :description "Elementary theory of partitions"
  :see-also '(05A17)
  :parent 11Pxx)

(define-msc-2010-classification 11P82
  :description "Analytic theory of partitions"
  :parent 11Pxx)

(define-msc-2010-classification 11P83
  :description "Partitions; congruences and congruential restrictions"
  :parent 11Pxx)

(define-msc-2010-classification 11P84
  :description "Partition identities; identities of Rogers-Ramanujan type"
  :parent 11Pxx)

(define-msc-2010-classification 11P99
  :description "None of the above, but in this section"
  :parent 11Pxx)

;; 11Rxx

(define-msc-2010-classification 11R04
  :description "Algebraic numbers; rings of algebraic integers"
  :parent 11Rxx)
(define-msc-2010-classification 11R06
  :description "PV-numbers and generalizations; other special algebraic numbers; Mahler measure"
  :parent 11Rxx)
(define-msc-2010-classification 11R09
  :description "Polynomials (irreducibility, etc.)"
  :parent 11Rxx)
(define-msc-2010-classification 11R11
  :description "Quadratic extensions"
  :parent 11Rxx)
(define-msc-2010-classification 11R16
  :description "Cubic and quartic extensions"
  :parent 11Rxx)
(define-msc-2010-classification 11R18
  :description "Cyclotomic extensions"
  :parent 11Rxx)
(define-msc-2010-classification 11R20
  :description "Other abelian and metabelian extensions"
  :parent 11Rxx)
(define-msc-2010-classification 11R21
  :description "Other number fields"
  :parent 11Rxx)
(define-msc-2010-classification 11R23
  :description "Iwasawa theory"
  :parent 11Rxx)
(define-msc-2010-classification 11R27
  :description "Units and factorization"
  :parent 11Rxx)
(define-msc-2010-classification 11R29
  :description "Class numbers, class groups, discriminants"
  :parent 11Rxx)
(define-msc-2010-classification 11R32
  :description "Galois theory"
  :parent 11Rxx)
(define-msc-2010-classification 11R33
  :description "Integral representations related to algebraic numbers; Galois module structure of rings of integers"
  :see-also '(20C10)
  :parent 11Rxx)
(define-msc-2010-classification 11R34
  :description "Galois cohomology"
  :see-also '(12Gxx 19A31)
  :parent 11Rxx)
(define-msc-2010-classification 11R37
  :description "Class field theory"
  :parent 11Rxx)
(define-msc-2010-classification 11R39
  :description "Langlands-Weil conjectures, nonabelian class field theory"
  :see-also '(11Fxx 22E55)
  :parent 11Rxx)
(define-msc-2010-classification 11R42
  :description "Zeta functions and $L$-functions of number fields"
  :see-also '(11M41 19F27)
  :parent 11Rxx)
(define-msc-2010-classification 11R44
  :description "Distribution of prime ideals"
  :see-also '(11N05)
  :parent 11Rxx)
(define-msc-2010-classification 11R45
  :description "Density theorems"
  :parent 11Rxx)
(define-msc-2010-classification 11R47
  :description "Other analytic theory"
  :see-also '(11Nxx)
  :parent 11Rxx)
(define-msc-2010-classification 11R52
  :description "Quaternion and other division algebras: arithmetic, zeta functions"
  :parent 11Rxx)
(define-msc-2010-classification 11R54
  :description "Other algebras and orders, and their zeta and $L$-functions"
  :see-also '(11S45 16Hxx 16Kxx)
  :parent 11Rxx)
(define-msc-2010-classification 11R56
  :description "Adèle rings and groups"
  :parent 11Rxx)
(define-msc-2010-classification 11R58
  :description "Arithmetic theory of algebraic function fields"
  :see-also '(14-XX)
  :parent 11Rxx)
(define-msc-2010-classification 11R60
  :description "Cyclotomic function fields (class groups, Bernoulli objects, etc.)"
  :parent 11Rxx)
(define-msc-2010-classification 11R65
  :description "Class groups and Picard groups of orders"
  :parent 11Rxx)
(define-msc-2010-classification 11R70
  :description "$K$-theory of global fields"
  :see-also '(19Fxx)
  :parent 11Rxx)
(define-msc-2010-classification 11R80
  :description "Totally real fields"
  :see-also '(12J15)
  :parent 11Rxx)
(define-msc-2010-classification 11R99
  :description "None of the above, but in this section"
  :parent 11Rxx)

;; 11Sxx

(define-msc-2010-classificiation 11S05
  :description "Polynomials"
  :parent 11Sxx)

(define-msc-2010-classificiation 11S15
  :description "Ramification and extension theory"
  :parent 11Sxx)

(define-msc-2010-classificiation 11S20
  :description "Galois theory"
  :parent 11Sxx)

(define-msc-2010-classificiation 11S23
  :description "Integral representations"
  :parent 11Sxx)

(define-msc-2010-classificiation 11S25
  :description "Galois cohomology"
  :see-also '(12Gxx 16H05)
  :parent 11Sxx)

(define-msc-2010-classificiation 11S31
  :description "Class field theory; $p$-adic formal groups"
  :see-also '(14L05)
  :parent 11Sxx)

(define-msc-2010-classificiation 11S37
  :description "Langlands-Weil conjectures, nonabelian class field theory"
  :see-also '(11Fxx 22E50)
  :parent 11Sxx)

(define-msc-2010-classificiation 11S40
  :description "Zeta functions and $L$-functions"
  :see-also '(11M41 19F27)
  :parent 11Sxx)

(define-msc-2010-classificiation 11S45
  :description "Algebras and orders, and their zeta functions"
  :see-also '(11R52 11R54 16Hxx 16Kxx)
  :parent 11Sxx)

(define-msc-2010-classificiation 11S70
  :description "$K$-theory of local fields"
  :see-also '(19Fxx)
  :parent 11Sxx)

(define-msc-2010-classificiation 11S80
  :description "Other analytic theory (analogues of beta and gamma functions, $p$-adic integration, etc.)"
  :parent 11Sxx)

(define-msc-2010-classificiation 11S82
  :description "Non-Archimedean dynamical systems"
  :see-also '(37Pxx)
  :parent 11Sxx)

(define-msc-2010-classificiation 11S85
  :description "Other nonanalytic theory"
  :parent 11Sxx)

(define-msc-2010-classificiation 11S90
  :description "Prehomogeneous vector spaces"
  :parent 11Sxx)

(define-msc-2010-classificiation 11S99
  :description "None of the above, but in this section"
  :parent 11Sxx)

;; 11Txx

(define-msc-2010-classification 11T06
  :description "Polynomials"
  :parent 11Txx)

(define-msc-2010-classification 11T22
  :description "Cyclotomy"
  :parent 11Txx)

(define-msc-2010-classification 11T23
  :description "Exponential sums"
  :parent 11Txx)

(define-msc-2010-classification 11T24
  :description "Other character sums and Gauss sums"
  :parent 11Txx)

(define-msc-2010-classification 11T30
  :description "Structure theory"
  :parent 11Txx)

(define-msc-2010-classification 11T55
  :description "Arithmetic theory of polynomial rings over finite fields"
  :parent 11Txx)

(define-msc-2010-classification 11T60
  :description "Finite upper half-planes"
  :parent 11Txx)

(define-msc-2010-classification 11T71
  :description "Algebraic coding theory; cryptography"
  :parent 11Txx)

(define-msc-2010-classification 11T99
  :description "None of the above, but in this section"
  :parent 11Txx)

;; 11Uxx

(define-msc-2010-classification 11U05
  :description "Decidability"
  :see-also '(03B25)
  :parent 11Uxx)

(define-msc-2010-classification 11U07
  :description "Ultraproducts"
  :see-also '(03C20)
  :parent 11Uxx)

(define-msc-2010-classification 11U09
  :description "Model theory"
  :see-also '(03Cxx)
  :parent 11Uxx)

(define-msc-2010-classification 11U10
  :description "Nonstandard arithmetic"
  :see-also '(03H15)
  :parent 11Uxx)

(define-msc-2010-classification 11U99
  :description "None of the above, but in this section"
  :parent 11Uxx)

;; 11Yxx

(define-msc-2010-classification 11Y05
  :description "Factorization"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y11
  :description "Primality"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y16
  :description "Algorithms; complexity"
  :see-also '(68Q25)
  :parent 11Yxx) 

(define-msc-2010-classification 11Y35
  :description "Analytic computations"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y40
  :description "Algebraic number theory computations"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y50
  :description "Computer solution of Diophantine equations"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y55
  :description "Calculation of integer sequences"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y60
  :description "Evaluation of constants"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y65
  :description "Continued fraction calculations"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y70
  :description "Values of arithmetic functions; tables"
  :parent 11Yxx) 

(define-msc-2010-classification 11Y99
  :description "None of the above, but in this section"
  :parent 11Yxx) 

;; 11Zxx

(define-msc-2010-classification 11Z05
  :description "Miscellaneous applications of number theory"
  :parent 11Zxx) 

(define-msc-2010-classification 11Z99
  :description "None of the above, but in this section"
  :parent 11Zxx) 
