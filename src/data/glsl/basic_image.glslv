attribute vec2 input_vertex_coords;
attribute vec2 input_tex_coords;
uniform mat4 global_transform;
uniform mat3 inverse_tpos;
uniform vec2 image_pixel_size;

varying vec2 tex_coords;
varying vec3 normal;

//outputs position, normal, and a set of uv coords

void main()
{
     float half_width = image_pixel_size.x / 2.0;
     float half_height = image_pixel_size.y / 2.0;
     vec4 vertex = vec4( input_vertex_coords.x * half_width, input_vertex_coords.y * half_height, 0.0, 1.0);
     mat4 final_matrix = gl_ProjectionMatrix * global_transform;
     gl_Position = vertex * final_matrix;
     normal = vec3( 0.0, 0.0, 1.0) * inverse_tpos;
     tex_coords = input_tex_coords;
}