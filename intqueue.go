package main
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

  type _state_3 struct {
    c chan interface{}
}
func init_state_3(c chan interface{}) *_state_3 { return &_state_3{ c } }
func (x *_state_3) Send(v interface{}) { x.c <- v }
func (x *_state_3) Recv() interface{} { return <-x.c }

  type _state_4 struct {
   c chan interface{}
   next *_state_0
}

func init_state_4(c chan interface{}) *_state_4 { return &_state_4{c, nil} }
func (x *_state_4) Send(v int) *_state_0 {
   if x.next == nil { x.next = init_state_0(x.c) }; x.c <- v; return x.next }
func (x *_state_4) Recv() (int, *_state_0) {
   if x.next == nil { x.next = init_state_0(x.c) }; return (<-x.c).(int), x.next }

  type _state_2 struct {
    c  chan interface{}
    ls map[string]interface{}
  }
  func init_state_2(c chan interface{}) *_state_2 { m := make(map[string]interface{})
 	m["some"] = init_state_4( c )
	m["none"] = init_state_3( c )
   return &_state_2{ c, m } }
func (x *_state_2) Send(v string) { x.c <- v }
func (x *_state_2) Recv() string  { return (<-x.c).(string) }

  type _state_0 struct {
    c  chan interface{}
    ls map[string]interface{}
  }
  func init_state_0(c chan interface{}) *_state_0 { m := make(map[string]interface{})
 	m["deq"] = init_state_2( c )
	m["enq"] = init_state_1( c )
   return &_state_0{ c, m } }
func (x *_state_0) Send(v string) { x.c <- v }
func (x *_state_0) Recv() string  { return (<-x.c).(string) }

  //Declaration list compilation
func elem(_x0 int) func (_x *_state_0, t *_state_0) {
 return func (_c0 *_state_0, t *_state_0){
for {
 label := _c0.Recv()
switch label {
case "deq" :
_c00 := _c0.ls["deq"].(*_state_2)
_c00.Send("some")
_c01 := _c00.ls["some"].(*_state_4)
_c02 := _c01.Send(_x0)
// FWD _c0 t Start
for {
t_c02 := _c02.Recv()
t.Send(t_c02)
switch t_c02 {
case "enq":
t0 := t.ls["enq"].(*_state_1)
_c03 := _c02.ls["enq"].(*_state_1)
t0_c03, _c03_t0 := _c03.Recv()
_c02 = _c03_t0
t = t0.Send(t0_c03)
case "deq":
t0 := t.ls["deq"].(*_state_2)
_c03 := _c02.ls["deq"].(*_state_2)
_c03t0 := t0.Recv()
_c03.Send(_c03t0)
switch _c03t0 {
case "none":
t1 := t0.ls["none"].(*_state_3)
_c04 := _c03.ls["none"].(*_state_3)
t1.Recv()
_c04.Send(nil)
return
case "some":
t1 := t0.ls["some"].(*_state_4)
_c04 := _c03.ls["some"].(*_state_4)
_c04t1, _c04_t1 := t1.Recv()
t = _c04_t1
_c02 = _c04.Send(_c04t1)
}
}
}
// FWD _c0 t End
case "enq" :
_c00 := _c0.ls["enq"].(*_state_1)
_x1, _c01 := _c00.Recv()
t.Send("enq")
t0 := t.ls["enq"].(*_state_1)
t1 := t0.Send(_x1)
//Update arguments
_x0 = _x0
//Update channels
_c0 = _c01
t = t1
 }
}
}
}
//Main compilation
func main () {
    m:= init_state_3(make (chan interface{}))
go func () {
m.Recv()
}()
func (m *_state_3){
m.Send(nil)
}(m)
}
