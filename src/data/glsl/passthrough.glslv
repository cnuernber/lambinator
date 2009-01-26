//Just pass through the vertexes unchanged from their
//original form
attribute vec2 input_vertex_coords;
attribute vec2 input_tex_coords;

varying vec2 tex_coords;

void main()
{    
     gl_Position = vec4( input_vertex_coords.x, input_vertex_coords.y, 0.0, 1.0);
     tex_coords = input_tex_coords;
}