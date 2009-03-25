uniform sampler2D tex;
uniform vec2 UVRange;
varying vec2 tex_coords;

void main()
{	
	vec2 range_coords = tex_coords * UVRange;
	vec4 tex_lookup = texture2D(tex,range_coords.st);
	gl_FragColor = tex_lookup;
}