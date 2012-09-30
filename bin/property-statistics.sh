#!/bin/bash -

set -u; # enable some extra checks, such as using uninitialized variables

# General dependence computation
depends_property () {
	grep " $1 " * | wc --lines;
}

# Relation properties: reflexivity, symmetry, asymmetry, connectedness, irreflexivity

depends_reflexivity() {
    depends_property 'Reflexivity';
}

depends_symmetry() {
    depends_property 'Symmetry';
}

depends_asymmetry() {
    depends_property 'Antisymmetry';
}

depends_connectedness() {
    depends_property 'Connectedness';
}

depends_irreflexivity() {
    depends_property 'Irreflexivity';
}

depends_asymmetry() {
    depends_property 'Antisymmetry';
}

# Function properties: projectivity, involutiveness, idempotence, commutativity

depends_projectivity() {
    depends_property 'Projectivity';
}

depends_involutiveness() {
    depends_property 'Involutiveness';
}

depends_idempotence() {
    depends_property 'Idempotence';
}

depends_commutativity() {
    depends_property 'Commutativity';
}

# Reporting

# Relation properties
echo -n "Number of items that depend on reflexivity of some constructor: "
depends_reflexivity
echo -n "Number of items that depend on symmetry of some constructor: "
depends_symmetry
echo -n "Number of items that depend on asymmetry of some constructor: "
depends_asymmetry
echo -n "Number of items that depend on connectedness of some constructor: "
depends_connectedness
echo -n "Number of items that depend on irreflexivity of some constructor: "
depends_irreflexivity

# Function properties
echo -n "Number of items that depend on projectivity of some constructor: "
depends_projectivity
echo -n "Number of items that depend on involutiveness of some constructor: "
depends_involutiveness
echo -n "Number of items that depend on idempotence of some constructor: "
depends_idempotence
echo -n "Number of items that depend on commutativity of some constructor: "
depends_commutativity

exit 0
