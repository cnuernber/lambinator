/* Fragment shader */
void main()
{
	gl_FragColor[0] = gl_FragCoord[0] / 500.0;
	gl_FragColor[1] = gl_FragCoord[1] / 500.0;
	gl_FragColor[2] = 1.0;
}
