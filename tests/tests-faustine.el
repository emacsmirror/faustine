(require 'faustine)

(describe "The feature"
  (it "can use bug and feature"
    (expect (featurize "bug" "feature")
            :to-equal
            "It's not a bug, it's a feature")))
