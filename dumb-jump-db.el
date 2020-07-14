(defcustom dumb-jump-find-rules
  '((:type function
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\((defun|cl-defun)\\s+" term word-boundary)
     :tests ("(defun test (blah)" "(defun test\n" "(cl-defun test (blah)" "(cl-defun test\n")
     :not ("(defun test-asdf (blah)" "(defun test-blah\n" "(cl-defun test-asdf (blah)"
           "(cl-defun test-blah\n"  "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\(defvar\\b\\s*" term word-boundary)
     :tests ("(defvar test " "(defvar test\n")
     :not ("(defvar tester" "(defvar test?" "(defvar test-"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\(defcustom\\b\\s*" term word-boundary)
     :tests ("(defcustom test " "(defcustom test\n")
     :not ("(defcustom tester" "(defcustom test?" "(defcustom test-"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\(setq\\b\\s*" term word-boundary)
     :tests ("(setq test 123)")
     :not ("setq test-blah 123)" "(setq tester" "(setq test?" "(setq test-"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elisp
     :regex ("\\\(" term "\\s+")
     :tests ("(let ((test 123)))")
     :not ("(let ((test-2 123)))"))

    ;; variable in method signature
    (:type variable
     :supports (ag rg git-grep)
     :language elisp
     :regex ("\\((defun|cl-defun)\\s*.+\\\(?\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(defun blah (test)" "(defun blah (test blah)" "(defun (blah test)")
     :not ("(defun blah (test-1)" "(defun blah (test-2 blah)" "(defun (blah test-3)"))

    ;; common lisp
    (:type function
     :supports (ag grep rg git-grep)
     :language commonlisp
     :regex ("\\\(defun\\s+" term word-boundary)
     :tests ("(defun test (blah)" "(defun test\n")
     :not ("(defun test-asdf (blah)" "(defun test-blah\n"
           "(defun tester (blah)" "(defun test? (blah)" "(defun test- (blah)"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language commonlisp
     :regex ("\\\(defparameter\\b\\s*" term word-boundary)
     :tests ("(defparameter test " "(defparameter test\n")
     :not ("(defparameter tester" "(defparameter test?" "(defparameter test-"))

    ;; racket
    (:type function
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(define\\s+\\(\\s*" term word-boundary)
     :tests ("(define (test blah)" "(define (test\n")
     :not ("(define test blah" "(define (test-asdf blah)" "(define test (lambda (blah"))
    (:type function
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(define\\s+" term "\\s*\\\(\\s*lambda")
     :tests ("(define test (lambda (blah" "(define test (lambda\n")
     :not ("(define test blah" "(define test-asdf (lambda (blah)" "(define (test)" "(define (test blah) (lambda (foo"))
    (:type function
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(let\\s+" term "\\s*(\\\(|\\\[)*")
     :tests ("(let test ((blah foo) (bar bas))" "(let test\n" "(let test [(foo")
     :not ("(let ((test blah"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(define\\s+" term word-boundary)
     :tests ("(define test " "(define test\n")
     :not ("(define (test"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("(\\\(|\\\[)\\s*" term "\\s+")
     :tests ("(let ((test 'foo" "(let [(test 'foo" "(let [(test 'foo" "(let [[test 'foo" "(let ((blah 'foo) (test 'bar)")
     :not ("{test foo"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(lambda\\s+\\\(?[^\(\)]*\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(lambda (test)" "(lambda (foo test)" "(lambda test (foo)")
     :not ("(lambda () test"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\\(define\\s+\\\([^\(\)]+\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(define (foo test)" "(define (foo test bar)")
     :not ("(define foo test" "(define (test foo" "(define (test)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language racket
     :regex ("\\(struct\\s+" term word-boundary)
     :tests ("(struct test (a b)"))

    ;; scheme
    (:type function
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(define\\s+\\(\\s*" term word-boundary)
     :tests ("(define (test blah)" "(define (test\n")
     :not ("(define test blah" "(define (test-asdf blah)" "(define test (lambda (blah"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(define\\s+" term "\\s*\\\(\\s*lambda")
     :tests ("(define test (lambda (blah" "(define test (lambda\n")
     :not ("(define test blah" "(define test-asdf (lambda (blah)" "(define (test)" "(define (test blah) (lambda (foo"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(let\\s+" term "\\s*(\\\(|\\\[)*")
     :tests ("(let test ((blah foo) (bar bas))" "(let test\n" "(let test [(foo")
     :not ("(let ((test blah"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(define\\s+" term word-boundary)
     :tests ("(define test " "(define test\n")
     :not ("(define (test"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("(\\\(|\\\[)\\s*" term "\\s+")
     :tests ("(let ((test 'foo" "(let [(test 'foo" "(let [(test 'foo" "(let [[test 'foo" "(let ((blah 'foo) (test 'bar)")
     :not ("{test foo"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(lambda\\s+\\\(?[^\(\)]*\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(lambda (test)" "(lambda (foo test)" "(lambda test (foo)")
     :not ("(lambda () test"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scheme
     :regex ("\\\(define\\s+\\\([^\(\)]+\\s*" term word-boundary "\\s*\\\)?")
     :tests ("(define (foo test)" "(define (foo test bar)")
     :not ("(define foo test" "(define (test foo" "(define (test)"))

    ;; c++
    (:type function
     :supports (ag rg git-grep)
     :language c++
     :regex ("\\b" term "(\\s|\\))*\\((\\w|[,&*.<>]|\\s)*(\\))\\s*(const|->|\\{|$)|typedef\\s+(\\w|[(*]|\\s)+" term "(\\)|\\s)*\\(")
     :tests ("int test(){" "my_struct (*test)(int a, int b){" "auto MyClass::test ( Builder& reference, ) -> decltype( builder.func() ) {" "int test( int *random_argument) const {" "test::test() {" "typedef int (*test)(int);")
     :not ("return test();)" "int test(a, b);" "if( test() ) {" "else test();"))
    ;; (:type variable :supports (grep) :language c++
    ;;        :regex "(\\b\\w+|[,>])([*&]|\\s)+JJJ\\s*(\\[([0-9]|\\s)*\\])*\\s*([=,){;]|:\\s*[0-9])|#define\\s+JJJ\\b"
    ;;        :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "unsigned int test:2;"))
    (:type variable
     :supports (ag rg)
     :language c++
     :regex ("\\b(?!(class\\b|struct\\b|return\\b|else\\b|delete\\b))(\\w+|[,>])([*&]|\\s)+" term "\\s*(\\[(\\d|\\s)*\\])*\\s*([=,(){;]|:\\s*\\d)|#define\\s+" term "\\b")
     :tests ("int test=2;" "char *test;" "int x = 1, test = 2" "int test[20];" "#define test" "typedef int test;" "unsigned int test:2")
     :not ("return test;" "#define NOT test" "else test=2;"))
    (:type type
     :supports (ag rg git-grep)
     :language c++
     :regex ("\\b(class|struct|enum|union)\\b\\s*" term "\\b\\s*(final\\s*)?(:((\\s*\\w+\\s*::)*\\s*\\w*\\s*<?(\\s*\\w+\\s*::)*\\w+>?\\s*,*)+)?((\\{|$))|}\\s*" term "\\b\\s*;")
     :tests ("typedef struct test {" "enum test {" "} test;" "union test {" "class test final: public Parent1, private Parent2{" "class test : public std::vector<int> {")
     :not("union test var;" "struct test function() {"))

    ;; clojure
    (:type variable
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(def\\s+" term word-boundary)
     :tests ("(def test (foo)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defn-?\\s+" term word-boundary)
     :tests ("(defn test [foo]" "(defn- test [foo]")
     :not ("(defn test? [foo]" "(defn- test? [foo]"))
    (:type function
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defmacro\\s+" term word-boundary)
     :tests ("(defmacro test [foo]"))
    (:type function
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(deftask\\s+" term word-boundary)
     :tests ("(deftask test [foo]"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(deftype\\s+" term word-boundary)
     :tests ("(deftype test [foo]"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defmulti\\s+" term word-boundary)
     :tests ("(defmulti test fn"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defmethod\\s+" term word-boundary)
     :tests ("(defmethod test type"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(definterface\\s+" term word-boundary)
     :tests ("(definterface test (foo)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defprotocol\\s+" term word-boundary)
     :tests ("(defprotocol test (foo)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language clojure
     :regex ("\\(defrecord\\s+" term word-boundary)
     :tests ("(defrecord test [foo]"))

    ;; coffeescript
    (:type function
     :supports (ag grep rg git-grep)
     :language coffeescript
     :regex ("^\\s*" term "\\s*[=:].*[-=]>")
     :tests ("test = ()  =>" "test= =>" "test = ->" "test=()->"
             "test : ()  =>" "test: =>" "test : ->" "test:()->")
     :not ("# test = =>" "test = 1"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language coffeescript
     :regex ("^\\s*" term "\\s*[:=][^:=-][^>]+$")
     :tests ("test = $" "test : [" "test = {" "test = a")
     :not ("test::a" "test: =>" "test == 1" "# test = 1"))
    (:type class
     :supports (ag grep rg git-grep)
     :language coffeescript
     :regex ("^\\s*\\bclass\\s+" term "")
     :tests ("class test" "class test extends")
     :not ("# class"))

    ;; obj-c
    (:type function
     :supports (ag grep rg git-grep)
     :language objc
     :regex ("\\\)\\s*" term "(:|\\b|\\s)")
     :tests ("- (void)test" "- (void)test:(UIAlertView *)alertView")
     :not ("- (void)testnot" "- (void)testnot:(UIAlertView *)alertView"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language objc
     :regex ("\\b\\*?" term "\\s*=[^=\\n]+")
     :tests ("NSString *test = @\"asdf\"")
     :not ("NSString *testnot = @\"asdf\"" "NSString *nottest = @\"asdf\""))
    (:type type
     :supports (ag grep rg git-grep)
     :language objc
     :regex ("(@interface|@protocol|@implementation)\\b\\s*" term "\\b\\s*")
     :tests ("@interface test: UIWindow")
     :not ("@interface testnon: UIWindow"))
    (:type type
     :supports (ag grep rg git-grep)
     :language objc
     :regex ("typedef\\b\\s+(NS_OPTIONS|NS_ENUM)\\b\\([^,]+?,\\s*" term "\\b\\s*")
     :tests ("typedef NS_ENUM(NSUInteger, test)")
     :not ("typedef NS_ENUMD(NSUInteger, test)"))

    ;; swift
    (:type variable
     :supports (ag grep rg git-grep)
     :language swift
     :regex ("(let|var)\\s*" term "\\s*(=|:)[^=:\\n]+")
     :tests ("let test = 1234" "var test = 1234" "private lazy var test: UITapGestureRecognizer") :not ("if test == 1234:"))
    (:type function
     :supports (ag grep rg git-grep)
     :language swift
     :regex ("func\\s*" term "\\b\\s*\\\(")
     :tests ("func test(asdf)" "func test()")
     :not ("func testnot(asdf)" "func testnot()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language swift
     :regex ("(class|struct)\\s*" term "\\b\\s*?")
     :tests ("class test:" "class test: UIWindow")
     :not ("class testnot:" "class testnot(object):"))

    ;; c#
    (:type function
     :supports (ag rg)
     :language csharp
     :regex ("^\\s*(?:[\\w\\[\\]]+\\s+){1,3}" term "\\s*\\\(")
     :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
             "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
     :not ("test()" "testnot()" "blah = new test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language csharp
     :regex ("\\s*\\b" term "\\s*=[^=\\n)]+")
     :tests ("int test = 1234")
     :not ("if test == 1234:" "int nottest = 44"))
    (:type type
     :supports (ag grep rg git-grep)
     :language csharp
     :regex ("(class|interface)\\s*" term "\\b")
     :tests ("class test:" "public class test : IReadableChannel, I")
     :not ("class testnot:" "public class testnot : IReadableChannel, I"))

    ;; java (literally the same regexes as c#, but different tests)
    (:type function
     :supports (ag rg)
     :language java
     :regex ("^\\s*(?:[\\w\\[\\]]+\\s+){1,3}" term "\\s*\\\(")
     :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
             "public static MyType test()" "private virtual SomeType test(param)" "static int test()"
             "private foo[] test()")
     :not ("test()" "testnot()" "blah = new test()" "foo bar = test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language java
     :regex ("\\s*\\b" term "\\s*=[^=\\n)]+")
     :tests ("int test = 1234")
     :not ("if test == 1234:" "int nottest = 44"))
    (:type type
     :supports (ag grep rg git-grep)
     :language java
     :regex ("(class|interface)\\s*" term "\\b")
     :tests ("class test:" "public class test implements Something")
     :not ("class testnot:" "public class testnot implements Something"))

    ;; vala (again just like c#, exactly the same..)
    (:type function
     :supports (ag rg)
     :language vala
     :regex ("^\\s*(?:[\\w\\[\\]]+\\s+){1,3}" term "\\s*\\\(")
     :tests ("int test()" "int test(param)" "static int test()" "static int test(param)"
             "public static MyType test()" "private virtual SomeType test(param)" "static int test()")
     :not ("test()" "testnot()" "blah = new test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language vala
     :regex ("\\s*\\b" term "\\s*=[^=\\n)]+")
     :tests ("int test = 1234")
     :not ("if test == 1234:" "int nottest = 44"))
    (:type type
     :supports (ag grep rg git-grep)
     :language vala
     :regex ("(class|interface)\\s*" term "\\b")
     :tests ("class test:" "public class test : IReadableChannel, I")
     :not ("class testnot:" "public class testnot : IReadableChannel, I"))

    ;; coq
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Variable\\s+" term "\\b")
     :tests ("Variable test")
     :not ("Variable testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Inductive\\s+" term "\\b")
     :tests ("Inductive test")
     :not ("Inductive testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Lemma\\s+" term "\\b")
     :tests ("Lemma test")
     :not ("Lemma testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Definition\\s+" term "\\b")
     :tests ("Definition test")
     :not ("Definition testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Hypothesis\\s+" term "\\b")
     :tests ("Hypothesis test")
     :not ("Hypothesis testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Theorm\\s+" term "\\b")
     :tests ("Theorm test")
     :not ("Theorm testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Fixpoint\\s+" term "\\b")
     :tests ("Fixpoint test")
     :not ("Fixpoint testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*Module\\s+" term "\\b")
     :tests ("Module test")
     :not ("Module testx"))
    (:type function
     :supports (ag rg git-grep)
     :language coq
     :regex ("\\s*CoInductive\\s+" term "\\b")
     :tests ("CoInductive test")
     :not ("CoInductive testx"))

    ;; python
    (:type variable
     :supports (ag grep rg git-grep)
     :language python
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test == 1234:" "_test = 1234"))
    (:type function
     :supports (ag grep rg git-grep)
     :language python
     :regex ("def\\s*" term "\\b\\s*\\\(")
     :tests ("\tdef test(asdf)" "def test()")
     :not ("\tdef testnot(asdf)" "def testnot()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language python
     :regex ("class\\s*" term "\\b\\s*\\\(?")
     :tests ("class test(object):" "class test:")
     :not ("class testnot:" "class testnot(object):"))

    ;; matlab
    (:type variable
     :supports (ag grep rg git-grep)
     :language matlab
     :regex ("^\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("for test = 1:2:" "_test = 1234"))
    (:type function
     :supports (ag grep rg git-grep)
     :language matlab
     :regex ("^\\s*function\\s*[^=]+\\s*=\\s*" term "\\b")
     :tests ("\tfunction y = test(asdf)" "function x = test()" "function [x, losses] = test(A, y, lambda, method, qtile)")
     :not ("\tfunction testnot(asdf)" "function testnot()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language matlab
     :regex ("^\\s*classdef\\s*" term "\\b\\s*")
     :tests ("classdef test")
     :not ("classdef testnot"))

    ;; nim
    (:type variable
     :supports (ag grep rg git-grep)
     :language nim
     :regex ("(const|let|var)\\s*" term "\\s*(=|:)[^=:\\n]+")
     :tests ("let test = 1234" "var test = 1234" "var test: Stat" "const test = 1234")
     :not ("if test == 1234:"))
    (:type function
     :supports (ag grep rg git-grep)
     :language nim
     :regex ("(proc|func|macro|template)\\s*`?" term "`?\\b\\s*\\\(")
     :tests ("\tproc test(asdf)" "proc test()" "func test()" "macro test()" "template test()")
     :not ("\tproc testnot(asdf)" "proc testnot()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language nim
     :regex ("type\\s*" term "\\b\\s*(\\{[^}]+\\})?\\s*=\\s*\\w+")
     :tests ("type test = object" "type test {.pure.} = enum")
     :not ("type testnot = object"))

    ;; nix
    (:type variable
     :supports (ag grep rg git-grep)
     :language nix
     :regex ("\\b\\s*" term "\\s*=[^=;]+")
     :tests ("test = 1234;" "test = 123;" "test=123")
     :not ("testNot = 1234;" "Nottest = 1234;" "AtestNot = 1234;"))

    ;; ruby
    (:type variable
     :supports (ag rg git-grep)
     :language ruby
     :regex ("^\\s*((\\w+[.])*\\w+,\\s*)*" term "(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)")
     :tests ("test = 1234" "self.foo, test, bar = args")
     :not ("if test == 1234" "foo_test = 1234"))
    (:type function
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*" term "($|[^\\w|:])")
     :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
             "def self.test()" "def MODULE::test()" "private def test")
     :not ("def test_foo"))
    (:type function
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|\\W)define(_singleton|_instance)?_method(\\s|[(])\\s*:" term "($|[^\\w|:])")
     :tests ("define_method(:test, &body)"
             "mod.define_instance_method(:test) { body }"))
    (:type type
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|[^\\w.])class\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("class test" "class Foo::test"))
    (:type type
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|[^\\w.])module\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("module test" "module Foo::test"))
    (:type function
     :supports (ag rg git-grep)
     :language ruby
     :regex ("(^|\\W)alias(_method)?\\W+" term "(\\W|$)")
     :tests ("alias test some_method"
             "alias_method :test, :some_method"
             "alias_method 'test' 'some_method'"
             "some_class.send(:alias_method, :test, :some_method)")
     :not ("alias some_method test"
           "alias_method :some_method, :test"
           "alias test_foo test"))

    ;; Groovy
    (:type variable
     :supports (ag rg git-grep)
     :language groovy
     :regex ("^\\s*((\\w+[.])*\\w+,\\s*)*" term "(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)")
     :tests ("test = 1234" "self.foo, test, bar = args")
     :not ("if test == 1234" "foo_test = 1234"))
    (:type function
     :supports (ag rg git-grep)
     :language groovy
     :regex ("(^|[^\\w.])((private|public)\\s+)?def\\s+(\\w+(::|[.]))*" term "($|[^\\w|:])")
     :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
             "def self.test()" "def MODULE::test()" "private def test")
     :not ("def test_foo"))
    (:type type :supports (ag rg git-grep) :language groovy
     :regex ("(^|[^\\w.])class\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("class test" "class Foo::test"))

    ;; crystal
    (:type variable
     :supports (ag rg git-grep)
     :language crystal
     :regex ("^\\s*((\\w+[.])*\\w+,\\s*)*" term "(,\\s*(\\w+[.])*\\w+)*\\s*=([^=>~]|$)")
     :tests ("test = 1234" "self.foo, test, bar = args")
     :not ("if test == 1234" "foo_test = 1234"))
    (:type function
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])((private|public|protected)\\s+)?def\\s+(\\w+(::|[.]))*" term "($|[^\\w|:])")
     :tests ("def test(foo)" "def test()" "def test foo" "def test; end"
             "def self.test()" "def MODULE::test()" "private def test")
     :not ("def test_foo"))
    (:type type
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])class\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("class test" "class Foo::test"))
    (:type type
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])module\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("module test" "module Foo::test"))
    (:type type
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])struct\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("struct test" "struct Foo::test"))
    (:type type
     :supports (ag rg git-grep)
     :language crystal
     :regex ("(^|[^\\w.])alias\\s+(\\w*::)*" term "($|[^\\w|:])")
     :tests ("alias test" "alias Foo::test"))

    ;; scad
    (:type variable
     :supports (ag grep rg git-grep)
     :language scad
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test == 1234 {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scad
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type module
     :supports (ag grep rg git-grep)
     :language scad
     :regex ("module\\s*" term "\\s*\\\(")
     :tests ("module test()" "module test ()"))

    ;; scala
    (:type variable
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("\\bval\\s*" term "\\s*=[^=\\n]+")
     :tests ("val test = 1234")
     :not ("case test => 1234"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("\\bvar\\s*" term "\\s*=[^=\\n]+")
     :tests ("var test = 1234")
     :not ("case test => 1234"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("\\btype\\s*" term "\\s*=[^=\\n]+")
     :tests ("type test = 1234")
     :not ("case test => 1234"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("\\bdef\\s*" term "\\s*\\\(")
     :tests ("def test(asdf)" "def test()"))
    (:type type
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("class\\s*" term "\\s*\\\(?")
     :tests ("class test(object)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("trait\\s*" term "\\s*\\\(?")
     :tests ("trait test(object)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language scala
     :regex ("object\\s*" term "\\s*\\\(?")
     :tests ("object test(object)"))

    ;; R
    (:type variable
     :supports (ag grep rg git-grep)
     :language r
     :regex ("\\b" term "\\s*=[^=><]")
     :tests ("test = 1234")
     :not ("if (test == 1234)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language r
     :regex ("\\b" term "\\s*<-\\s*function\\b")
     :tests ("test <- function" "test <- function(")
     :not   ("test <- functionX"))

    ;; perl
    (:type function
     :supports (ag grep rg git-grep)
     :language perl
     :regex ("sub\\s*" term "\\s*(\\{|\\()")
     :tests ("sub test{" "sub test {" "sub test(" "sub test ("))
    (:type variable
     :supports (ag grep rg git-grep)
     :language perl
     :regex ("" term "\\s*=\\s*")
     :tests ("$test = 1234"))

    ;; shell
    :type function
    :supports (ag grep rg git-grep)
    :language shell
    :regex ("function\\s*" term "\\s*")
    :tests ("function test{" "function test {" "function test () {")
    :not   ("function nottest {")
    (:type function
     :supports (ag grep rg git-grep)
     :language shell
     :regex ("" term "\\\(\\\)\\s*\\{")
     :tests ("test() {")
     :not ("testx() {"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language shell
     :regex ("\\b" term "\\s*=\\s*")
     :tests ("test = 1234") :not ("blahtest = 1234"))

    ;; php
    (:type function
     :supports (ag grep rg git-grep)
     :language php
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language php
     :regex ("\\*\\s@method\\s+[^   ]+\\s+" term "\\(")
     :tests ("/** @method string|false test($a)" " * @method bool test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language php
     :regex ("(\\s|->|\\$|::)" term "\\s*=\\s*")
     :tests ("$test = 1234" "$foo->test = 1234"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language php
     :regex ("\\*\\s@property(-read|-write)?\\s+([^     ]+\\s+)&?\\$" term "(\\s+|$)")
     :tests ("/** @property string $test" "/** @property string $test description for $test property"  " * @property-read bool|bool $test" " * @property-write \\ArrayObject<string,resource[]> $test"))
    (:type trait
     :supports (ag grep rg git-grep)
     :language php
     :regex ("trait\\s*" term "\\s*\\\{")
     :tests ("trait test{" "trait test {"))
    (:type interface
     :supports (ag grep rg git-grep)
     :language php
     :regex ("interface\\s*" term "\\s*\\\{")
     :tests ("interface test{" "interface test {"))
    (:type class
     :supports (ag grep rg git-grep)
     :language php
     :regex ("class\\s*" term "\\s*(extends|implements|\\\{)")
     :tests ("class test{" "class test {" "class test extends foo" "class test implements foo"))

    ;; dart
    (:type function
     :supports (ag grep rg git-grep)
     :language dart
     :regex ("\\b" term "\\s*\\([^()]*\\)\\s*[{]")
     :tests ("test(foo) {" "test (foo){" "test(foo){"))
    (:type function
     :supports (ag grep rg git-grep)
     :language dart
     :regex ("class\\s*" term "\\s*[\\\(\\\{]")
     :tests ("class test(object) {" "class test{"))

    ;; faust
    (:type function
     :supports (ag grep rg git-grep)
     :language faust
     :regex ("\\b" term "\(\\\(.+\\\)\)*\\s*=")
     :tests ("test = osc + 0.5;" "test(freq) = osc(freq) + 0.5;"))

    ;; fortran
    (:type variable
     :supports (ag grep rg git-grep)
     :language fortran
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if (test == 1234)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language fortran
     :regex ("\\b(function|subroutine|FUNCTION|SUBROUTINE)\\s+" term "\\b\\s*\\\(")
     :tests ("function test (foo)" "integer function test(foo)"
             "subroutine test (foo, bar)" "FUNCTION test (foo)"
             "INTEGER FUNCTION test(foo)" "SUBROUTINE test (foo, bar)")
     :not ("end function test" "end subroutine test" "END FUNCTION test"
           "END SUBROUTINE test"))
    (:type function
     :supports (ag grep rg git-grep)
     :language fortran
     :regex ("^\\s*(interface|INTERFACE)\\s+" term "\\b")
     :tests ("interface test" "INTERFACE test")
     :not ("interface test2" "end interface test" "INTERFACE test2"
           "END INTERFACE test"))
    (:type type
     :supports (ag grep rg git-grep)
     :language fortran
     :regex ("^\\s*(module|MODULE)\\s+" term "\\s*")
     :tests ("module test" "MODULE test")
     :not ("end module test" "END MODULE test"))

    ;; go
    (:type variable
     :supports (ag grep rg git-grep)
     :language go
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test == 1234 {"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language go
     :regex ("\\s*\\b" term "\\s*:=\\s*")
     :tests ("test := 1234"))
    (:type function
     :supports (ag grep rg git-grep)
     :language go
     :regex ("func\\s+\\\([^\\\)]*\\\)\\s+" term "\\s*\\\(")
     :tests ("func (s *blah) test(filename string) string {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language go
     :regex ("func\\s+" term "\\s*\\\(")
     :tests ("func test(url string) (string, error)"))
    (:type type
     :supports (ag grep rg git-grep)
     :language go
     :regex ("type\\s+" term "\\s+struct\\s+\\\{")
     :tests ("type test struct {"))

    ;; javascript extended
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("(service|factory)\\\(['\"]" term "['\"]")
     :tags ("angular")
     :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\b" term "\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>")
     :tags ("es6")
     :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\b" term "\\s*\\([^()]*\\)\\s*[{]")
     :tags ("es6")
     :tests ("test(foo) {" "test (foo){" "test(foo){")
     :not ("test = blah.then(function(){"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :tags ("es6")
     :regex ("class\\s*" term "\\s*[\\\(\\\{]")
     :tests ("class test(object) {" "class test{"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :tags ("es6")
     :regex ("class\\s*" term "\\s+extends")
     :tests ("class test extends Component{"))

    ;; hcl terraform
    (:type block
     :supports (ag grep rg git-grep)
     :language hcl
     :regex ("(variable|output|module)\\s*\"" term "\"\\s*\\\{")
     :tests ("variable \"test\" {"
             "output \"test\" {"
             "module \"test\" {"))
    (:type block
     :supports (ag grep rg git-grep)
     :language hcl
     :regex ("(data|resource)\\s*\"\\w+\"\\s*\"" term "\"\\s*\\\{")
     :tests ("data \"openstack_images_image_v2\" \"test\" {"
             "resource \"google_compute_instance\" \"test\" {"))

    ;; javascript
    (:type variable
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234" "const test = props =>")
     :not ("if (test === 1234)"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\b" term "\\b\\s*,?\\s*\\\)?")
     :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
     :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
           "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\b" term "\\s*:\\s*function\\s*\\\(")
     :tests ("test: function()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language javascript
     :regex ("\\b" term "\\s*=\\s*function\\s*\\\(")
     :tests ("test = function()"))

    ;; typescript
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("(service|factory)\\\(['\"]" term "['\"]")
     :tags ("angular")
     :tests ("module.factory('test', [\"$rootScope\", function($rootScope) {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\b" term "\\s*[=:]\\s*\\\([^\\\)]*\\\)\\s+=>")
     :tests ("const test = (foo) => " "test: (foo) => {" "  test: (foo) => {"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\b" term "\\s*\\([^()]*\\)\\s*[{]")
     :tests ("test(foo) {" "test (foo){" "test(foo){")
     :not ("test = blah.then(function(){"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("class\\s*" term "\\s*[\\\(\\\{]")
     :tests ("class test{"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("class\\s*" term "\\s+extends")
     :tests ("class test extends Component{"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\b" term "\\s*:\\s*function\\s*\\\(")
     :tests ("test: function()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\b" term "\\s*=\\s*function\\s*\\\(")
     :tests ("test = function()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234" "const test = props =>")
     :not ("if (test === 1234)"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language typescript
     :regex ("\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\b" term "\\b\\s*,?\\s*\\\)?")
     :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah) {" "function(blah, test)")
     :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah) {" "function(blah, testLast)"
           "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah) {" "function(blah, Lasttest)"))

    ;; julia
    (:type function
     :supports (ag grep rg git-grep)
     :language julia
     :regex ("(@noinline|@inline)?\\s*function\\s*" term "(\\{[^\\}]*\\})?\\(")
     :tests ("function test()" "@inline function test()"
             "function test{T}(h)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language julia
     :regex ("(@noinline|@inline)?" term "(\\{[^\\}]*\\})?\\([^\\)]*\\)\s*=")
     :tests ("test(a)=1" "test(a,b)=1*8"
             "@noinline test()=1" "test{T}(x)=x"))
    (:type function
     :supports (ag grep rg git-grep)
     :language julia
     :regex ("macro\\s*" term "\\(")
     :tests ("macro test(a)=1" " macro test(a,b)=1*8"))
    (:type variable
     :supports (ag rg)
     :language julia
     :regex ("const\\s+" term "\\b")
     :tests ("const test = "))
    (:type type
     :supports (ag rg)
     :language julia
     :regex ("(mutable)?\\s*struct\\s*" term "")
     :tests ("struct test"))
    (:type type
     :supports (ag rg)
     :language julia
     :regex ("(type|immutable|abstract)\\s*" term "")
     :tests ("type test" "immutable test" "abstract test <:Testable" ))

    ;; haskell
    (:type module
     :supports (ag)
     :language haskell
     :regex ("^module\\s+" term "\\s+")
     :tests ("module Test (exportA, exportB) where"))
                                        ; TODO Doesn't support any '=' in arguments. E.g. 'foo A{a = b,..} = bar'.
    (:type top level function
     :supports (ag)
     :language haskell
     :regex ("^\\b" term "(?!(\\s+::))\\s+((.|\\s)*?)=\\s+")
     :tests ("test n = n * 2"
             "test X{..} (Y a b c) \n bcd \n =\n x * y"
             "test ab cd e@Datatype {..} (Another thing, inTheRow) = \n undefined"
             "test = runRealBasedMode @ext @ctx identity identity"
             "test unwrap wrap nr@Naoeu {..} (Action action, specSpecs) = \n    undefined")
     :not ("nottest n = n * 2"
           "let testnot x y = x * y" "test $ y z" "let test a o = mda"
           "test :: Sometype -> AnotherType aoeu kek = undefined"))
    (:type type-like
     :supports (ag)
     :language haskell
     :regex ("^\\s*((data(\\s+family)?)|(newtype)|(type(\\s+family)?))\\s+" term "\\s+")
     :tests ("newtype Test a = Something { b :: Kek }"
             "data Test a b = Somecase a | Othercase b"
             "type family Test (x :: *) (xs :: [*]) :: Nat where"
             "data family Test "
             "type Test = TestAlias")
     :not ("newtype NotTest a = NotTest (Not a)"
           "data TestNot b = Aoeu"))
                                        ; datatype contstuctor that doesn't match type definition.
    (:type (data)type constructor 1
     :supports (ag)
     :language haskell
     :regex ("(data|newtype)\\s{1,3}(?!" term "\\s+)([^=]{1,40})=((\\s{0,3}" term "\\s+)|([^=]{0,500}?((?<!(-- ))\\|\\s{0,3}" term "\\s+)))")
     :tests ("data Something a = Test { b :: Kek }"
             "data Mem a = TrueMem { b :: Kek } | Test (Mem Int) deriving Mda"
             "newtype SafeTest a = Test (Kek a) deriving (YonedaEmbedding)")
     :not ("data Test = Test { b :: Kek }"))
    (:type data/newtype record field
     :supports (ag)
     :language haskell
     :regex ("(data|newtype)([^=]*)=[^=]*?({([^=}]*?)(\\b" term ")\\s+::[^=}]+})")
     :tests ("data Mem = Mem { \n mda :: A \n  , test :: Kek \n , \n aoeu :: E \n }"
             "data Mem = Mem { \n test :: A \n  , mda :: Kek \n , \n aoeu :: E \n }"
             "data Mem = Mem { \n mda :: A \n  , aoeu :: Kek \n , \n test :: E \n }"
             "data Mem = Mem { test :: Kek } deriving Mda"
             "data Mem = Mem { \n test :: Kek \n } deriving Mda"
             "newtype Mem = Mem { \n test :: Kek \n } deriving (Eq)"
             "newtype Mem = Mem { -- | Some docs \n test :: Kek -- ^ More docs } deriving Eq"
             "newtype Mem = Mem { test :: Kek } deriving (Eq,Monad)"
             "newtype NewMem = OldMem { test :: [Tx] }"
             "newtype BlockHeaderList ssc = BHL\n { test :: ([Aoeu a], [Ssss])\n    } deriving (Eq)")
     :not ("data Heh = Mda { sometest :: Kek, testsome :: Mem }"))
    (:type typeclass
     :supports (ag)
     :language haskell
     :regex ("^class\\s+(.+=>\\s*)?" term "\\s+")
     :tests ("class (Constr1 m, Constr 2) => Test (Kek a) where"
             "class  Test  (Veryovka a)  where ")
     :not ("class Test2 (Kek a) where"
           "class MakeTest (AoeuTest x y z) where"))

    ;; ocaml
    (:type type
     :supports (ag rg)
     :language ocaml
     :regex ("^\\s*(and|type)\\s+.*\\b" term "\\b")
     :tests ("type test ="
             "and test ="
             "type 'a test ="
             "type ('a, _, 'c) test"))
    (:type variable
     :supports (ag rg)
     :language ocaml
     :regex ("let\\s+" term "\\b")
     :tests ("let test ="
             "let test x y ="))
    (:type variable
     :supports (ag rg)
     :language ocaml
     :regex ("let\\s+rec\\s+" term "\\b")
     :tests ("let rec test ="
             "let rec  test x y ="))
    (:type variable
     :supports (ag rg)
     :language ocaml
     :regex ("\\s*val\\s*\\b" term "\\b\\s*")
     :tests ("val test"))
    (:type module
     :supports (ag rg)
     :language ocaml
     :regex ("^\\s*module\\s*\\b" term "\\b")
     :tests ("module test ="))
    (:type module
     :supports (ag rg)
     :language ocaml
     :regex ("^\\s*module\\s*type\\s*\\b" term "\\b")
     :tests ("module type test ="))

    ;; lua
    (:type variable
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("\\s*\\b" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test === 1234"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("\\bfunction\\b[^\\(]*\\\(\\s*[^\\)]*\\b" term "\\b\\s*,?\\s*\\\)?")
     :tests ("function (test)" "function (test, blah)" "function somefunc(test, blah)" "function(blah, test)")
     :not ("function (testLen)" "function (test1, blah)" "function somefunc(testFirst, blah)" "function(blah, testLast)"
           "function (Lentest)" "function (blahtest, blah)" "function somefunc(Firsttest, blah)" "function(blah, Lasttest)"))
    (:type function
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("function\\s*" term "\\s*\\\(")
     :tests ("function test()" "function test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("function\\s*.+[.:]" term "\\s*\\\(")
     :tests ("function MyClass.test()" "function MyClass.test ()"
             "function MyClass:test()" "function MyClass:test ()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("\\b" term "\\s*=\\s*function\\s*\\\(")
     :tests ("test = function()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language lua
     :regex ("\\b.+\\." term "\\s*=\\s*function\\s*\\\(")
     :tests ("MyClass.test = function()"))

    ;; rust
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\blet\\s+(\\\([^=\\n]*)?(mut\s+)?" term "([^=\\n]*\\\))?(:\\s*[^=\\n]+)?\\s*=\\s*[^=\\n]+")
     :tests ("let test = 1234;"
             "let test: u32 = 1234;"
             "let test: Vec<u32> = Vec::new();"
             "let mut test = 1234;"
             "let mut test: Vec<u32> = Vec::new();"
             "let (a, test, b) = (1, 2, 3);"
             "let (a, mut test, mut b) = (1, 2, 3);"
             "let (mut a, mut test): (u32, usize) = (1, 2);"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bconst\\s+" term ":\\s*[^=\\n]+\\s*=[^=\\n]+")
     :tests ("const test: u32 = 1234;"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bstatic\\s+(mut\\s+)?" term ":\\s*[^=\\n]+\\s*=[^=\\n]+")
     :tests ("static test: u32 = 1234;"
             "static mut test: u32 = 1234;"))
    ;; variable in method signature
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bfn\\s+.+\\s*\\\((.+,\\s+)?" term ":\\s*[^=\\n]+\\s*(,\\s*.+)*\\\)")
     :tests ("fn abc(test: u32) -> u32 {"
             "fn abc(x: u32, y: u32, test: Vec<u32>, z: Vec<Foo>)"
             "fn abc(x: u32, y: u32, test: &mut Vec<u32>, z: Vec<Foo>)"))
    ;; "if let" and "while let" desugaring
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("(if|while)\\s+let\\s+([^=\\n]+)?(mut\\s+)?" term "([^=\\n\\\(]+)?\\s*=\\s*[^=\\n]+")
     :tests ("if let Some(test) = abc() {"
             "if let Some(mut test) = abc() {"
             "if let Ok(test) = abc() {"
             "if let Ok(mut test) = abc() {"
             "if let Foo(mut test) = foo {"
             "if let test = abc() {"
             "if let Some(test) = abc()"
             "if let Some((a, test, b)) = abc()"
             "while let Some(test) = abc() {"
             "while let Some(mut test) = abc() {"
             "while let Ok(test) = abc() {"
             "while let Ok(mut test) = abc() {")
     :not ("while let test(foo) = abc() {"))
    ;; structure fields
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("struct\\s+[^\\n{]+[{][^}]*(\\s*" term "\\s*:\\s*[^\\n},]+)[^}]*}")
     :tesst ("struct Foo { abc: u32, test: Vec<String>, b: PathBuf }"
             "struct Foo<T>{test:Vec<T>}"
             "struct FooBar<'a> { test: Vec<String> }")
     :not ("struct Foo { abc: u32, b: Vec<String> }"
           "/// ... construct the equivalent ...\nfn abc() {\n"))
    ;; enum variants
    (:type variable
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("enum\\s+[^\\n{]+\\s*[{][^}]*\\b" term "\\b[^}]*}")
     :tests ("enum Foo { VariantA, test, VariantB(u32) }"
             "enum Foo<T> { test(T) }"
             "enum BadStyle{test}"
             "enum Foo32 { Bar, testing, test(u8) }")
     :not ("enum Foo { testing }"))
    (:type function
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bfn\\s+" term "\\s*\\\(")
     :tests ("fn test(asdf: u32)" "fn test()" "pub fn test()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\bmacro_rules!\\s+" term "")
     :tests ("macro_rules! test"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("struct\\s+" term "\\s*[{\\\(]?")
     :tests ("struct test(u32, u32)"
             "struct test;"
             "struct test { abc: u32, def: Vec<String> }"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("trait\\s+" term "\\s*[{]?")
     :tests ("trait test;" "trait test { fn abc() -> u32; }"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("\\btype\\s+" term "([^=\\n]+)?\\s*=[^=\\n]+;")
     :tests ("type test<T> = Rc<RefCell<T>>;"
             "type test = Arc<RwLock<Vec<u32>>>;"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("impl\\s+((\\w+::)*\\w+\\s+for\\s+)?(\\w+::)*" term "\\s+[{]?")
     :tests ("impl test {"
             "impl abc::test {"
             "impl std::io::Read for test {"
             "impl std::io::Read for abc::test {"))
    (:type type
     :supports (ag grep rg git-grep)
     :language rust
     :regex ("mod\\s+" term "\\s*[{]?")
     :tests ("mod test;" "pub mod test {"))

    ;; elixir
    (:type function
     :supports (ag grep rg git-grep)
     :language elixir
     :regex ("\\bdef(p)?\\s+" term "\\s*[ ,\\\(]")
     :tests ("def test do"
             "def test, do:"
             "def test() do"
             "def test(), do:"
             "def test(foo, bar) do"
             "def test(foo, bar), do:"
             "defp test do"
             "defp test(), do:"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language elixir
     :regex ("\\s*" term "\\s*=[^=\\n]+")
     :tests ("test = 1234")
     :not ("if test == 1234"))
    (:type module
     :supports (ag grep rg git-grep)
     :language elixir
     :regex ("defmodule\\s+(\\w+\\.)*" term "\\s+")
     :tests ("defmodule test do"
             "defmodule Foo.Bar.test do"))
    (:type module
     :supports (ag grep rg git-grep)
     :language elixir
     :regex ("defprotocol\\s+(\\w+\\.)*" term "\\s+")
     :tests ("defprotocol test do"
             "defprotocol Foo.Bar.test do"))

    ;; erlang
    (:type function
     :supports (ag grep rg git-grep)
     :language erlang
     :regex ("^" term "\\b\\s*\\\(")
     :tests ("test() ->"
             "test()->"
             "test(Foo) ->"
             "test (Foo,Bar) ->"
             "test(Foo, Bar)->"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language erlang
     :regex ("\\s*" term "\\s*=[^:=\\n]+")
     :tests ("test = 1234")
     :not ("if test =:= 1234"
           "if test == 1234"))
    (:type module
     :supports (ag grep rg git-grep)
     :language erlang
     :regex ("^-module\\\(" term "\\\)")
     :tests ("-module(test)."))

    ;; scss
    (:type function
     :supports (ag grep rg git-grep)
     :language scss
     :regex ("@mixin\\s" term "\\b\\s*\\\(")
     :tests ("@mixin test()"))
    (:type function
     :supports (ag grep rg git-grep)
     :language scss
     :regex ("@function\\s" term "\\b\\s*\\\(")
     :tests ("@function test()"))
    (:type variable :supports (ag grep rg git-grep) :language scss
     :regex ("" term "\\s*:\\s*")
     :tests ("test  :"))

    ;; sml
    (:type type
     :supports (ag grep rg git-grep)
     :language sml
     :regex ("\\s*(data)?type\\s+.*\\b" term "\\b")
     :tests ("datatype test ="
             "datatype test="
             "datatype 'a test ="
             "type test ="
             "type 'a test ="
             "type 'a test"
             "type test")
     :not ("datatypetest ="))
    (:type variable
     :supports (ag grep rg git-grep)
     :language sml
     :regex ("\\s*val\\s+\\b" term "\\b")
     :tests ("val test ="
             "val test="
             "val test : bool"))
    (:type function
     :supports (ag grep rg git-grep)
     :language sml
     :regex ("\\s*fun\\s+\\b" term "\\b.*\\s*=")
     :tests ("fun test list ="
             "fun test (STRING_NIL, a) ="
             "fun test ((s1,s2): 'a queue) : 'a * 'a queue ="
             "fun test (var : q) : int ="
             "fun test f e xs ="))
    (:type module
     :supports (ag grep rg git-grep)
     :language sml
     :regex ("\\s*(structure|signature|functor)\\s+\\b" term "\\b")
     :tests ("structure test ="
             "structure test : MYTEST ="
             "signature test ="
             "functor test (T:TEST) ="
             "functor test(T:TEST) ="))

    ;; sql
    (:type function
     :supports (ag grep rg git-grep)
     :language sql
     :regex ("(CREATE|create)\\s+(.+?\\s+)?(FUNCTION|function|PROCEDURE|procedure)\\s+" term "\\s*\\\(")
     :tests ("CREATE FUNCTION test(i INT) RETURNS INT"
             "create or replace function test (int)"
             "CREATE PROCEDURE test (OUT p INT)"
             "create definer = 'test'@'localhost' procedure test()"))
    (:type table
     :supports (ag grep rg git-grep)
     :language sql
     :regex ("(CREATE|create)\\s+(.+?\\s+)?(TABLE|table)(\\s+(IF NOT EXISTS|if not exists))?\\s+" term "\\b")
     :tests ("CREATE TABLE test ("
             "create temporary table if not exists test"
             "CREATE TABLE IF NOT EXISTS test ("
             "create global temporary table test"))
    (:type view
     :supports (ag grep rg git-grep)
     :language sql
     :regex ("(CREATE|create)\\s+(.+?\\s+)?(VIEW|view)\\s+" term "\\b")
     :tests ("CREATE VIEW test ("
             "create sql security definer view test"
             "CREATE OR REPLACE VIEW test AS foo"))
    (:type type
     :supports (ag grep rg git-grep)
     :language sql
     :regex ("(CREATE|create)\\s+(.+?\\s+)?(TYPE|type)\\s+" term "\\b")
     :tests ("CREATE TYPE test"
             "CREATE OR REPLACE TYPE test AS foo ("
             "create type test as ("))

    ;; systemverilog
    (:type type
     :supports (ag grep rg git-grep)
     :language systemverilog
     :regex ("\\s*class\\s+\\b" term "\\b")
     :tests ("virtual class test;" "class test;" "class test extends some_class")
     :not ("virtual class testing;" "class test2;" "class some_test" "class some_class extends test"))
    (:type type
     :supports (ag grep rg git-grep)
     :language systemverilog
     :regex ("\\s*task\\s+\\b" term "\\b")
     :tests ("task test (" "task test(")
     :not ("task testing (" "task test2("))
    (:type type
     :supports (ag grep rg git-grep)
     :language systemverilog
     :regex ("\\s*\\b" term "\\b\\s*=")
     :tests ("assign test ="
             "assign test="
             "int test ="
             "int test=")
     :not ("assign testing =" "assign test2="))
    (:type function
     :supports (ag rg git-grep)
     :language systemverilog
     :regex ("function\\s[^\\s]+\\s*\\b" term "\\b")
     :tests ("function Matrix test ;"
             "function Matrix test;")
     :not ("function test blah"))
    ;; matches SV class handle declarations
    (:type function
     :supports (ag rg git-grep)
     :language systemverilog
     :regex ("^\\s*[^\\s]*\\s*[^\\s]+\\s+\\b" term "\\b")
     :tests ("some_class_name test"
             "  another_class_name  test ;"
             "some_class test[];"
             "some_class #(1) test")
     :not ("test some_class_name" "class some_class extends test"))

    ;; vhdl
    (:type type
     :supports (ag grep rg git-grep)
     :language vhdl
     :regex ("\\s*type\\s+\\b" term "\\b")
     :tests ("type test is" "type test  is")
     :not ("type testing is" "type test2  is"))
    (:type type
     :supports (ag grep rg git-grep)
     :language vhdl
     :regex ("\\s*constant\\s+\\b" term "\\b")
     :tests ("constant test :" "constant test:")
     :not ("constant testing " "constant test2:"))
    (:type function
     :supports (ag grep rg git-grep)
     :language vhdl
     :regex ("function\\s*\"?" term "\"?\\s*\\\(")
     :tests ("function test(signal)"
             "function test (signal)"
             "function \"test\" (signal)")
     :not ("function testing(signal"))

    ;; latex
    (:type command
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\.*newcommand\\\*?\\s*\\\{\\s*(\\\\)" term "\\s*}")
     :tests ("\\newcommand{\\test}"
             "\\renewcommand{\\test}"
             "\\renewcommand*{\\test}"
             "\\newcommand*{\\test}"
             "\\renewcommand{ \\test }")
     :not("\\test"  "test"))
    (:type command
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\.*newcommand\\\*?\\s*(\\\\)" term word-boundary)
     :tests ("\\newcommand\\test {}"
             "\\renewcommand\\test{}"
             "\\newcommand \\test")
     :not("\\test"  "test"))
    (:type length
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\(s)etlength\\s*\\\{\\s*(\\\\)" term "\\s*}")
     :tests ("\\setlength { \\test}"
             "\\setlength{\\test}"
             "\\setlength{\\test}{morecommands}" )
     :not("\\test"  "test"))
    (:type counter
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\newcounter\\\{\\s*" term "\\s*}")
     :tests ("\\newcounter{test}" )
     :not("\\test"  "test"))
    (:type environment
     :supports (ag grep rg git-grep)
     :language tex
     :regex ("\\\\.*newenvironment\\s*\\\{\\s*" term "\\s*}")
     :tests ("\\newenvironment{test}"
             "\\newenvironment {test}{morecommands}"
             "\\lstnewenvironment{test}"
             "\\newenvironment {test}" )
     :not("\\test"  "test" ))

    ;; pascal (todo: var, type, const)
    (:type function
     :supports (ag grep rg git-grep)
     :language pascal
     :regex ("\\bfunction\\s+" term "\\b")
     :tests ("  function test : "))
    (:type function
     :supports (ag grep rg git-grep)
     :language pascal
     :regex ("\\bprocedure\\s+" term "\\b")
     :tests ("  procedure test ; "))

    ;; f#
    (:type variable
     :supports (ag grep git-grep)
     :language fsharp
     :regex ("let\\s+" term "\\b.*\\\=")
     :tests ("let test = 1234" "let test() = 1234" "let test abc def = 1234")
     :not ("let testnot = 1234"
           "let testnot() = 1234"
           "let testnot abc def = 1234"))
    (:type interface
     :supports (ag grep git-grep)
     :language fsharp
     :regex ("member(\\b.+\\.|\\s+)" term "\\b.*\\\=")
     :tests ("member test = 1234"
             "member this.test = 1234")
     :not ("member testnot = 1234"
           "member this.testnot = 1234"))
    (:type type
     :supports (ag grep git-grep)
     :language fsharp
     :regex ("type\\s+" term "\\b.*\\\=")
     :tests ("type test = 1234")
     :not ("type testnot = 1234"))

    ;; kotlin
    (:type function
     :supports (ag grep rg git-grep)
     :language kotlin
     :regex ("fun\\s*(<[^>]*>)?\\s*" term "\\s*\\(")
     :tests ("fun test()" "fun <T> test()"))
    (:type variable
     :supports (ag grep rg git-grep)
     :language kotlin
     :regex ("(val|var)\\s*" term "\\b")
     :not ("val testval" "var testvar")
     :tests ("val test " "var test"))
    (:type type
     :supports (ag grep rg git-grep)
     :language kotlin
     :regex ("(class|interface)\\s*" term "\\b")
     :tests ("class test" "class test : SomeInterface" "interface test"))

    ;; protobuf
    (:type message
     :supports (ag grep rg git-grep)
     :language protobuf
     :regex ("message\\s+" term "\\s*\\\{")
     :tests ("message test{" "message test {"))
    (:type enum
     :supports (ag grep rg git-grep)
     :language protobuf
     :regex ("enum\\s+" term "\\s*\\\{")
     :tests ("enum test{" "enum test {")))
  "List of regex patttern templates organized by language and type to use for generating the grep command."
  :type '(repeat (plist :options ((:type string)
                                  (:supports string)
                                  (:language string)
                                  (:regex string)
                                  (:tests (repeat string))
                                  (:not (repeat string)))))
  :group 'dumb-jump)

(defcustom dumb-jump-language-file-exts
  ;; https://github.com/ggreer/the_silver_searcher/blob/master/tests/list_file_types.t
  ;; https://github.com/BurntSushi/ripgrep/blob/master/ignore/src/types.rs#L99
  '((:language elisp :ext "el" :agtype "elisp" :rgtype "elisp")
    (:language elisp :ext "el.gz" :agtype "elisp" :rgtype "elisp")
    (:language commonlisp :ext "lisp" :agtype "lisp" :rgtype "lisp")
    (:language commonlisp :ext "lsp" :agtype "lisp" :rgtype "lisp")
    (:language c++ :ext "c" :agtype "cc" :rgtype "c")
    (:language c++ :ext "h" :agtype "cc" :rgtype "c")
    (:language c++ :ext "C" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "H" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "tpp" :agtype "cpp")
    (:language c++ :ext "cpp" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "hpp" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "cxx" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "hxx" :agtype "cpp")
    (:language c++ :ext "cc" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "hh" :agtype "cpp" :rgtype "cpp")
    (:language c++ :ext "c++")
    (:language c++ :ext "h++")
    (:language coq :ext "v")
    (:language ocaml :ext "ml" :agtype "ocaml" :rgtype "ocaml")
    (:language ocaml :ext "mli" :agtype "ocaml" :rgtype "ocaml")
    (:language ocaml :ext "mll" :agtype "ocaml" :rgtype "ocaml")
    (:language ocaml :ext "mly" :agtype "ocaml" :rgtype "ocaml")
    ;; groovy is nil type because jenkinsfile is not in searcher type lists
    (:language groovy :ext "gradle")
    (:language groovy :ext "groovy")
    (:language groovy :ext "jenkinsfile")
    (:language haskell :ext "hs" :agtype "haskell" :rgtype "haskell")
    (:language haskell :ext "lhs" :agtype "haskell" :rgtype "haskell")
    (:language objc :ext "m" :agtype "objc" :rgtype "objc")
    (:language csharp :ext "cs" :agtype "csharp" :rgtype "csharp")
    (:language java :ext "java" :agtype "java" :rgtype "java")
    (:language vala :ext "vala" :agtype "vala" :rgtype "vala")
    (:language vala :ext "vapi" :agtype "vala" :rgtype "vala")
    (:language julia :ext "jl" :agtype "julia" :rgtype "julia")
    (:language clojure :ext "clj" :agtype "clojure" :rgtype "clojure")
    (:language clojure :ext "cljc" :agtype "clojure" :rgtype "clojure")
    (:language clojure :ext "cljs" :agtype "clojure" :rgtype "clojure")
    (:language clojure :ext "cljx" :agtype "clojure" :rgtype "clojure")
    (:language coffeescript :ext "coffee" :agtype "coffee" :rgtype "coffeescript")
    (:language faust :ext "dsp")
    (:language faust :ext "lib")
    (:language fortran :ext "F" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f77" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f90" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f95" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "F77" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "F90" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "F95" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "f03" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "for" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "ftn" :agtype "fortran" :rgtype "fortran")
    (:language fortran :ext "fpp" :agtype "fortran" :rgtype "fortran")
    (:language go :ext "go" :agtype "go" :rgtype "go")
    (:language javascript :ext "js" :agtype "js" :rgtype "js")
    (:language javascript :ext "jsx" :agtype "js" :rgtype "js")
    (:language javascript :ext "vue" :agtype "js" :rgtype "js")
    (:language javascript :ext "html" :agtype "html" :rgtype "html")
    (:language javascript :ext "css" :agtype "css" :rgtype "css")
    (:language typescript :ext "ts" :agtype "ts" :rgtype "ts")
    (:language typescript :ext "tsx" :agtype "ts" :rgtype "ts")
    (:language typescript :ext "vue" :agtype "ts" :rgtype "ts")
    (:language dart :ext "dart":rgtype "dart")
    (:language lua :ext "lua" :agtype "lua" :rgtype "lua")
    ;; the extension "m" is also used by obj-c so must use matlab-mode
    ;; since obj-c will win by file extension, but here for searcher types
    (:language matlab :ext "m" :agtype "matlab" :rgtype "matlab")
    (:language nim :ext "nim" :agtype "nim" :rgtype "nim")
    (:language nix :ext "nix" :agtype "nix" :rgtype "nix")
    (:language org :ext "org" :rgtype "org")
    (:language perl :ext "pl" :agtype "perl" :rgtype "perl")
    (:language perl :ext "pm" :agtype "perl" :rgtype "perl")
    (:language perl :ext "pm6" :agtype "perl")
    (:language perl :ext "perl" :rgtype "perl")
    (:language perl :ext "plh" :rgtype "perl")
    (:language perl :ext "plx" :rgtype "perl")
    (:language perl :ext "pod" :agtype "perl" :rgtype "pod")
    (:language perl :ext "t" :agtype "perl")
    (:language php :ext "php" :agtype "php" :rgtype "php")
    (:language php :ext "php3" :agtype "php" :rgtype "php")
    (:language php :ext "php4" :agtype "php" :rgtype "php")
    (:language php :ext "php5" :agtype "php" :rgtype "php")
    (:language php :ext "phtml" :agtype "php" :rgtype "php")
    (:language php :ext "inc" :agtype "php")
    (:language python :ext "py" :agtype "python" :rgtype "py")
    (:language r :ext "R" :agtype "r" :rgtype "r")
    (:language r :ext "r" :agtype "r" :rgtype "r")
    (:language r :ext "Rmd" :agtype "r" :rgtype "r")
    (:language r :ext "Rnw" :agtype "r" :rgtype "r")
    (:language r :ext "Rtex" :agtype "r")
    (:language r :ext "Rrst" :agtype "r")
    (:language racket :ext "rkt" :agtype "racket" :rgtype "lisp")
    (:language crystal :ext "cr" :agtype "crystal" :rgtype "crystal")
    (:language crystal :ext "ecr" :agtype "crystal")
    (:language ruby :ext "rb" :agtype "ruby" :rgtype "ruby")
    (:language ruby :ext "erb" :agtype "ruby")
    (:language ruby :ext "haml" :agtype "ruby")
    (:language ruby :ext "rake" :agtype "ruby")
    (:language ruby :ext "slim" :agtype "ruby")
    (:language rust :ext "rs" :agtype "rust" :rgtype "rust")
    (:language scad :ext "scad")
    (:language scala :ext "scala" :agtype "scala" :rgtype "scala")
    (:language scheme :ext "scm" :agtype "scheme" :rgtype "lisp")
    (:language scheme :ext "ss" :agtype "scheme" :rgtype "lisp")
    (:language scheme :ext "sld" :agtype "scheme" :rgtype "lisp")
    (:language shell :ext "sh")
    (:language shell :ext "bash")
    (:language shell :ext "csh")
    (:language shell :ext "ksh")
    (:language shell :ext "tcsh")
    (:language sml :ext "sml" :agtype "sml" :rgtype "sml")
    (:language sql :ext "sql" :agtype "sql" :rgtype "sql")
    (:language swift :ext "swift" :rgtype "swift")
    (:language tex :ext "tex" :agtype "tex" :rgtype "tex")
    (:language elixir :ext "ex" :agtype "elixir" :rgtype "elixir")
    (:language elixir :ext "exs" :agtype "elixir" :rgtype "elixir")
    (:language elixir :ext "eex" :agtype "elixir" :rgtype "elixir")
    (:language erlang :ext "erl" :agtype "erlang" :rgtype "erlang")
    (:language systemverilog :ext "sv" :agtype "verilog" :rgtype "verilog")
    (:language systemverilog :ext "svh" :agtype "verilog" :rgtype "verilog")
    (:language vhdl :ext "vhd" :agtype "vhdl" :rgtype "vhdl")
    (:language vhdl :ext "vhdl" :agtype "vhdl" :rgtype "vhdl")
    (:language scss :ext "scss" :agtype "css" :rgtype "css")
    (:language pascal :ext "pas" :agtype "delphi")
    (:language pascal :ext "dpr" :agtype "delphi")
    (:language pascal :ext "int" :agtype "delphi")
    (:language pascal :ext "dfm" :agtype "delphi")
    (:language fsharp :ext "fs" :agtype "fsharp")
    (:language fsharp :ext "fsi" :agtype "fsharp")
    (:language fsharp :ext "fsx" :agtype "fsharp")
    (:language kotlin :ext "kt" :agtype "kotlin" :rgtype "kotlin")
    (:language kotlin :ext "kts" :agtype "kotlin" :rgtype "kotlin")
    (:language protobuf :ext "proto" :agtype "proto" :rgtype "protobuf")
    (:language hcl :ext "tf" :agtype "terraform" :rgtype "tf")
    (:language hcl :ext "tfvars" :agtype "terraform"))

  "Mapping of programming language(s) to file extensions."
  :type '(repeat (plist :options ((:language string :tag "Language")
                                  (:ext (string :tag "Extension"))
                                  (:agtype (string :tag "Ag type"))
                                  (:rgtype (string :tag "Ripgrep type")))))
  :group 'dumb-jump)

(defcustom dumb-jump-language-contexts
  '((:language javascript :type function :right "^(")
    (:language javascript :type variable :left "($")
    (:language javascript :type variable :right "^)" :left "($")
    (:language javascript :type variable :right "^\\.")
    (:language javascript :type variable :right "^;")
    (:language typescript :type function :right "^(")
    (:language perl :type function :right "^(")
    (:language php :type function :right "^(")
    (:language php :type class :left "new\s+")
    (:language elisp :type function :left "($")
    (:language elisp :type variable :right "^)")
    (:language scheme :type function :left "($")
    (:language scheme :type variable :right "^)"))
  "List of under points contexts for each language.
This helps limit the number of regular expressions we use
if we know that if there's a '(' immediately to the right of
a symbol then it's probably a function call"
  :type '(repeat (plist :options ((:language (symbol :tag Language))
                                  (:type (choice (const function)
                                                 (const variable)))
                                  (:left (choice (const :tag Anything nil)
                                                 (string :tag Regular expression)))
                                  (:right (choice (const :tag Anything nil)
                                                  (string :tag Regular expression))))))
  :group 'dumb-jump)

(defcustom dumb-jump-comments-alist
  '((c++ . "//")
    (elisp . ";")
    (commonlisp . ";")
    (javascript . "//")
    (typescript . "//")
    (dart . "//")
    (haskell . "--")
    (lua . "--")
    (rust . "//")
    (julia . "#")
    (objc . "//")
    (csharp . "//")
    (java . "//")
    (clojure . ";")
    (coffeescript . "#")
    (faust . "//")
    (fortran . "!")
    (go . "//")
    (perl . "#")
    (php . "//")
    (python . "#")
    (matlab . "%")
    (r . "#")
    (racket . ";")
    (ruby . "#")
    (crystal . "#")
    (nim . "#")
    (nix . "#")
    (scala . "//")
    (scheme . ";")
    (shell . "#")
    (swift . "//")
    (elixir . "#")
    (erlang . "%")
    (tex . "%")
    (systemverilog . "//")
    (vhdl . "--")
    (scss . "//")
    (pascal . "//")
    (protobuf . "//")
    (hcl . "#"))
  "List of one-line comments organized by language."
  :type '(alist :key-type symbol :value-type string)
  :group 'dumb-jump)

(provide 'dumb-jump-db)
