struct Intersection {
    pos: vec4f,
    valid: bool,
    
  };

fn intersect_segment_plane(a: vec3f, b: vec3f, p0: vec3f, n_unit: vec3f) -> Intersection {
    let dir = b - a;
    let denom = dot(n_unit, dir);
    let eps = 1e-6;

    var out: Intersection;
    out.valid = false;

    if (abs(denom) < eps) { 
        return out;
    }

    let t = dot(n_unit, (p0 - a)) / denom;

        if (t < 0.0 || t > 1.0) { return out; }

    out.valid = true;
    out.pos = vec4f(a + t * dir, 1.0);
    return out;
}


@group(0) @binding(0) var<uniform> camera_matrix: mat4x4f; // Uniforms;
@group(0) @binding(1) var<uniform> inverse_camera_matrix: mat4x4f; // Uniforms;
@group(0) @binding(2) var<uniform> gridSize: f32;

  struct MyVSOutput {
    @builtin(position) pos: vec4f,
 @location(0) pos2: vec3f,
    
  };

  struct FSOutput {
    @location(0) color: vec4f,
    @builtin(frag_depth) depth: f32,
    
  };

@vertex
fn vs_main(@builtin(vertex_index) vidx: u32) ->  MyVSOutput {

    var i = vidx*3u;

    let quadPos = array(
      vec2f(-1, -1),
      vec2f( 1, -1),
      vec2f(-1,  1),
      vec2f(-1,  1),
      vec2f( 1, -1),
      vec2f( 1,  1),
    );

    var vsOut: MyVSOutput;
    
    vsOut.pos = vec4f(quadPos[vidx],0 , 1);
    vsOut.pos2 = vsOut.pos.xyz;
    
    return vsOut;
}

@fragment
fn fs_main(fsInput: MyVSOutput) -> FSOutput {
    var p0 = inverse_camera_matrix * vec4f(fsInput.pos2.xy, 0, 1.0);
    var p1 = inverse_camera_matrix * vec4f(fsInput.pos2.xy, 1, 1.0);

    var out: FSOutput;
    var p = intersect_segment_plane(p0.xyz/ p0.w, p1.xyz/ p1.w, vec3f(0,0,0), vec3f(0,1,0));
    
    if (!p.valid ){ 
        discard;
    }
    
    let lineWidth = 0.03;

    let pclipspace = camera_matrix* p.pos;
    out.depth =  (pclipspace.z/ pclipspace.w);

    let uv = p.pos.xz/gridSize;
    
    // largely based on https://bgolus.medium.com/the-best-darn-grid-shader-yet-727f9278b9d8
    let uvDeriv : vec2f = fwidth(uv);

    let drawWidth : vec2f = clamp(vec2f(lineWidth), uvDeriv, vec2f(0.5)); //
    let lineAA :vec2f = uvDeriv * 1.5;
    let gridUV :vec2f = 1.0 - abs(fract(uv) * 2.0 - 1.0);
    var grid2 :vec2f = smoothstep(drawWidth + lineAA, drawWidth - lineAA, gridUV);

    grid2 *= saturate(lineWidth / drawWidth);
    var grid :f32 = mix(grid2.x, 1.0, grid2.y);

    let fadeThreshold = 0.5;
    let fadeFactor = 1.0 - smoothstep( 0, fadeThreshold, max(uvDeriv.x, uvDeriv.y));

    out.color = vec4f(0.5, 0.5, 0.5, grid*fadeFactor);


    return out;

}
