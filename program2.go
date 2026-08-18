package main
import "fmt"
//Preamble generation
type _state_1 struct {
   c chan interface{}
   next *_state_0
}

func init_state_1(c chan interface{}) *_state_1 { return &_state_1{c, nil} }
func (x *_state_1) Send(v int) *_state_0 {
   if x.next == nil { x.next = init_state_0(x.c) }; x.c <- v; return x.next }
func (x *_state_1) Recv() (int, *_state_0) {
   if x.next == nil { x.next = init_state_0(x.c) }; return (<-x.c).(int), x.next }

  type _state_2 struct {
    c chan interface{}
}
func init_state_2(c chan interface{}) *_state_2 { return &_state_2{ c } }
func (x *_state_2) Send(v interface{}) { x.c <- v }
func (x *_state_2) Recv() interface{} { return <-x.c }

  type _state_0 struct {
    c  chan interface{}
    ls map[string]interface{}
  }
  func init_state_0(c chan interface{}) *_state_0 { m := make(map[string]interface{})
 	m["stop"] = init_state_2( c )
	m["next"] = init_state_1( c )
   return &_state_0{ c, m } }
func (x *_state_0) Send(v string) { x.c <- v }
func (x *_state_0) Recv() string  { return (<-x.c).(string) }

  //Declaration list compilation
func fib(_x0 int) func (_x int) func (_x *_state_0) {
 return func (_x1 int) func (_x *_state_0){
return func (_c0 *_state_0){
label := _c0.Recv()
switch label {
case "stop" :
_c00 := _c0.ls["stop"].(*_state_2)
_c00.Send(nil)
case "next" :
_c00 := _c0.ls["next"].(*_state_1)
_c01 := _c00.Send(_x1)
fib(_x1)(_x1)(_c01)
}
}}
}
//Main compilation
func main () {
    m:= init_state_2(make (chan interface{}))
go func () {
m.Recv()
}()
func (m *_state_2){
f := init_state_0(make(chan interface{}))
go fib(0)(1)(f)
f.Send("next")
f0 := f.ls["next"].(*_state_1)
x1, f1 := f0.Recv()
fmt.Printf("%v\n",x1)
f1.Send("next")
f2 := f1.ls["next"].(*_state_1)
x2, f3 := f2.Recv()
fmt.Printf("%v\n",x2)
f3.Send("next")
f4 := f3.ls["next"].(*_state_1)
x3, f5 := f4.Recv()
fmt.Printf("%v\n",x3)
f5.Send("stop")
f6 := f5.ls["stop"].(*_state_2)
f6.Recv()
m.Send(nil)
}(m)
}
