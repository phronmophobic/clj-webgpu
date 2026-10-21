
struct Uniforms {
matrix: mat4x4f,
};


@group(0) @binding(0) var<uniform> camera_matrix: mat4x4f; // Uniforms;
@group(0) @binding(1) var<storage, read> v1: array<f32>;
@group(0) @binding(2) var<storage, read> normals: array<f32>;


@group(0) @binding(3) var<uniform> model_matrix: mat4x4f;

struct OurVertexShaderOutput {
    @builtin(position) position: vec4f,
 @location(0) normal: vec3f,

};

@vertex
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> OurVertexShaderOutput {

    var i = in_vertex_index*3u;

    var vsOutput: OurVertexShaderOutput;
    var originalPosition = vec4f ( v1[i], v1[i+1], v1[i+2], 1.0);

    vsOutput.position = camera_matrix * model_matrix * originalPosition;
    var normal = (model_matrix * vec4f(normals[i], normals[i+1], normals[i+2],0)).xyz;
    vsOutput.normal = normal;

    return vsOutput;
}

@fragment
fn fs_main(fsInput: OurVertexShaderOutput) -> @location(0) vec4f {

    var color = vec4f(0.0, 1.0, 0.0, 1.0);

     var lightFactor = 0.5 + 0.5*dot(fsInput.normal, normalize(vec3f(1.0, 1.0, -1.0f)));

//    if ( lightFactor < 0.3 ){
//        lightFactor = 0.3;
//    }

    return vec4f(color.rgb * lightFactor, 0.6);
}
