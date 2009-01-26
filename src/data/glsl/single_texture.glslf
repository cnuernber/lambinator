uniform sampler2D tex;
varying vec2 tex_coords;

void main()
{	
	vec4 tex_lookup = texture2D(tex,tex_coords.st);
	gl_FragColor = tex_lookup;
}