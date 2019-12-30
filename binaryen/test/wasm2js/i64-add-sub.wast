;; Testing i64 lowering for addition and subtraction.

(module
  (func $dummy)

  (func (export "check_add_i64") (param $0 i64) (param $1 i64) (param $r i64) (result i32)
    (i64.eq (i64.add (local.get $0) (local.get $1)) (local.get $r)))

  (func (export "check_sub_i64") (param $0 i64) (param $1 i64) (param $r i64) (result i32)
    (i64.eq (i64.sub (local.get $0) (local.get $1)) (local.get $r)))
)

(assert_return (invoke "check_add_i64" (i32.const 0) (i32.const 0)
                                       (i32.const 0) (i32.const 0)
                                       (i32.const 0) (i32.const 0))
               (i32.const 1))
(assert_return (invoke "check_add_i64" (i32.const 1) (i32.const 0)
                                       (i32.const 0) (i32.const 0)
                                       (i32.const 1) (i32.const 0))
               (i32.const 1))
(assert_return (invoke "check_add_i64" (i32.const 0) (i32.const 0)
                                       (i32.const 1) (i32.const 0)
                                       (i32.const 1) (i32.const 0))
               (i32.const 1))
(assert_return (invoke "check_add_i64" (i32.const 0) (i32.const 1)
                                       (i32.const 0) (i32.const 0)
                                       (i32.const 0) (i32.const 1))
               (i32.const 1))
(assert_return (invoke "check_add_i64" (i32.const 0) (i32.const 0)
                                       (i32.const 0) (i32.const 1)
                                       (i32.const 0) (i32.const 1))
               (i32.const 1))
(assert_return (invoke "check_add_i64" (i32.const 0xffffffff) (i32.const 0)
                                       (i32.const 1) (i32.const 0)
                                       (i32.const 0) (i32.const 1))
               (i32.const 1))

;; subtraction
(assert_return (invoke "check_sub_i64" (i32.const 0) (i32.const 0)
                                       (i32.const 0) (i32.const 0)
                                       (i32.const 0) (i32.const 0))
               (i32.const 1))
(assert_return (invoke "check_sub_i64" (i32.const 1) (i32.const 0)
                                       (i32.const 0) (i32.const 0)
                                       (i32.const 1) (i32.const 0))
               (i32.const 1))
(assert_return (invoke "check_sub_i64" (i32.const 0) (i32.const 0)
                                       (i32.const 1) (i32.const 0)
                                       (i32.const 0xffffffff) (i32.const 0xffffffff))
               (i32.const 1))
(assert_return (invoke "check_sub_i64" (i32.const 0) (i32.const 1)
                                       (i32.const 0) (i32.const 0)
                                       (i32.const 0) (i32.const 1))
               (i32.const 1))
(assert_return (invoke "check_sub_i64" (i32.const 0) (i32.const 0)
                                       (i32.const 0) (i32.const 1)
                                       (i32.const 0) (i32.const 0xffffffff))
               (i32.const 1))
(assert_return (invoke "check_sub_i64" (i32.const 0xffffffff) (i32.const 0)
                                       (i32.const 1) (i32.const 0)
                                       (i32.const 0xfffffffe) (i32.const 0))
               (i32.const 1))
(assert_return (invoke "check_sub_i64" (i32.const 0) (i32.const 1)
                                       (i32.const 1) (i32.const 1)
                                       (i32.const 0xffffffff) (i32.const 0xffffffff))
               (i32.const 1))