
struct Uniforms {
matrix: mat4x4f,
};


@group(0) @binding(0) var<uniform> camera_matrix: mat4x4f;
// @group(0) @binding(1) var<storage, read> v1: array<f32>;

@group(0) @binding(1) var<storage, read> triVerts : array<i32>;

@group(0) @binding(2) var<storage, read> xyz : array<f32>;

@group(0) @binding(3) var<storage, read> normals: array<f32>;


@group(0) @binding(4) var<uniform> model_matrix: mat4x4f;

struct OurVertexShaderOutput {
    @builtin(position) position: vec4f,
 @location(0) normal: vec3f,

};

@vertex
fn vs_main(@builtin(vertex_index) vertex_index: u32) -> OurVertexShaderOutput {

//    var i = in_vertex_index*3u;

    let cornerIndex = triVerts[vertex_index];

    var vsOutput: OurVertexShaderOutput;
    var originalPosition = vec4f ( xyz[cornerIndex*3 + 0], xyz[cornerIndex*3 + 1], xyz[cornerIndex*3 + 2], 1.0);

    vsOutput.position = camera_matrix * model_matrix * originalPosition;
//    var normal = (model_matrix * vec4f(normals[i], normals[i+1], normals[i+2],0)).xyz;
    

    let normalIndex = vertex_index / 3;
     vsOutput.normal = vec3f ( normals[normalIndex*3 + 0], normals[normalIndex*3 + 1], normals[normalIndex*3 + 2]);

    return vsOutput;
}

@fragment
fn fs_main(fsInput: OurVertexShaderOutput) -> @location(0) vec4f {

    var color = vec4f(0.0, 1.0, 0.0, 1.0);

//    var lightFactor = 1.0;

     var lightFactor = 0.5 + 0.5*dot(normalize(fsInput.normal), normalize(vec3f(1.0, 1.0, -1.0f)));

    return vec4f(color.rgb * lightFactor, 1.0);
}
