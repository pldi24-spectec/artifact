(module
  (memory 0)
  (memory 0)
  (memory 0)
  (memory 0)
  (memory $m 0)
  
  (func (export "size") (result i32) (memory.size $m))
  (func (export "grow") (param $sz i32) (drop (memory.grow $m (local.get $sz))))
)

(assert_return (invoke "size") (i32.const 0))
(assert_return (invoke "grow" (i32.const 1)))
(assert_return (invoke "size") (i32.const 1))
(assert_return (invoke "grow" (i32.const 4)))
(assert_return (invoke "size") (i32.const 5))
(assert_return (invoke "grow" (i32.const 0)))
(assert_return (invoke "size") (i32.const 5))

