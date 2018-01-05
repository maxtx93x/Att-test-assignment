(ns att-test-assignment.matcher-test
  (:require [clojure.test :refer :all]
            [att-test-assignment.matcher :refer [new-pattern recognize]]))

(deftest new-pattern-test
  (testing "Should create new pattern"
    (let [pattern (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")]
      (is (every? pattern [:host :path :queryparam])))))

(deftest recognize-test
  (let [pattern (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")
        multiparam-pattern (new-pattern "host(dribbble.com); path(shots/?id); queryparam(offset=?offset); queryparam(list=?type);")]
    (testing "Should recognize url by pattern"
     (let [url "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"
           result (recognize pattern url)]
       (is (= result [[:id "1905065-Travel-Icons-pack"] [:offset "1"]]))))
    (testing "Should check hostname"
      (let [url "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"
            result (recognize pattern url)]
        (is (= result nil))))
    (testing "Should check path"
      (let [url "https://dribbble.com/wrongpath/1905065-Travel-Icons-pack?offset=1"
            result (recognize pattern url)]
        (is (= result nil))))
    (testing "Should check queryparam"
      (let [url "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users"
            result (recognize pattern url)]
        (is (= result nil))))
    (testing "Should support multiple queryparams"
      (let [url "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1"
            result (recognize multiparam-pattern url)]
        (is (= result [[:id "1905065-Travel-Icons-pack"] [:offset "1"] [:type "users"]]))))))

