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
@group(0) @binding(2) var<storage, read> verts: array<f32>;

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
      vec2f(0, 0),
      vec2f(1, 0),
      vec2f(0, 1),
      vec2f(0, 1),
      vec2f(1, 0),
      vec2f(1, 1),
    );

    var vsOut: MyVSOutput;
    
    vsOut.pos = vec4f(verts[i], verts[i+1], verts[i+2], 1);
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
        out.depth = 10;
        out.color = vec4f(0);
        return out;

    }
    
    let width = 0.03;

    let pclipspace = camera_matrix* p.pos;
    out.depth =  (pclipspace.z/ pclipspace.w);

    let onXAxis = abs(p.pos.x) < width;
    if ( onXAxis ){
        out.color = vec4f(0, 0, 0, 1 - abs(p.pos.x)/width);
        return out;
    }    
    let onZAxis = abs(p.pos.z) < width;
    if ( onZAxis ){
        out.color = vec4f(0, 0, 0, 1 - abs(p.pos.z)/width);
        return out;
    }    


    let onLineX = abs(p.pos.x % 1) < width || abs(p.pos.x % 1) > 1 - width;
    let onLineZ = abs(p.pos.z % 1) < width || abs(p.pos.z % 1) > 1 - width;

    out.color = vec4f(0.5, 0.5, 0.5, select(0.0, 1.0, onLineX || onLineZ));
    
    return out;

}
