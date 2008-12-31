uniform sampler2D tex;
varying vec2 tex_coords;

void main()
{	
	gl_FragColor = texture2D(tex,tex_coords.st);
}