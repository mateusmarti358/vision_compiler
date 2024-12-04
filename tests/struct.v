use io from lib

struct vec3
  x: f32
  y: f32
  z: f32
end

func vec3::move_forward do
  self.z = self.z - 1.0
end
 
func vec3::move_left do
  self.x = self.x - 1.0
end

func vec3::print do
  println("pos: ${float}, ${float}, ${float}", self.x, self.y, self.z)
end

struct entity
  pos: vec3
  health: i32
  name: str
end

func entity::print do
  println("name: ${str}\nhealth: ${int}\n", self.name, self.health)
  self.pos.print()
end

func main do
  player: entity = entity { pos: vec3 { x: 5.0, y: 5.0, z: 5.0 }, health: 100, name: 'Player' }

  player.print()

  player.pos.move_forward()

  player.pos.move_left()

  player.print()
end
