package a
module a where
func Plus_one(n int) func (_x *_state_0) {
 return func (c *_state_0){
c0 := c.Send((n + 1))
c0.Send(nil)
}}
func Plus_two(n int) func (_x *_state_0) {
 return func (c *_state_0){
d := init_state_0(make(chan interface{}))
go plus_one((n + 1))(d)
// FWD c d Start
cd, d0 := d.Recv()
c0 := c.Send(cd)
d0.Recv()
c0.Send(nil)
return
// FWD c d End
}}
func main () {
m := init_state_1(make (chan interface{}))
go func () {
m.Recv()
}()
func (m *_state_1){
d := init_state_0(make(chan interface{}))
go plus_two(1)(d)
a, d0 := d.Recv()
fmt.Printf("%v\n",a)
d0.Recv()
m.Send(nil)
}(m)
}
