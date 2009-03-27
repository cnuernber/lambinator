uniform sampler2D tex;
varying vec3 normal;
varying vec2 tex_coords;


void main()
{
	vec4 tex_lookup = texture2D(tex, tex_coords.st);
	gl_FragColor = vec4(1.0 - tex_lookup);
}