//Just pass through the vertexes unchanged from their
//original form
attribute vec2 input_tex_coords;

varying vec2 tex_coords;

void main()
{    
     gl_Position = ftransform();
     tex_coords = input_tex_coords;
}