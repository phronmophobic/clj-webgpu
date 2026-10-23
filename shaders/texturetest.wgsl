
@group(0) @binding(0) var<uniform> camera: mat4x4f;
@group(0) @binding(1) var<uniform> model_matrix: mat4x4f;
@group(0) @binding(2) var<storage, read> verts: array<f32>;
@group(0) @binding(3) var<storage, read> normals: array<f32>;

@group(0) @binding(4) var ourTexture: texture_2d<f32>;
@group(0) @binding(5) var ourSampler: sampler;

struct OurVertexShaderOutput {
    @builtin(position) position: vec4f,
    @location(0) texcoord: vec2f,
    @location(1) normal: vec3f,
};

@vertex
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> OurVertexShaderOutput {
    var i = in_vertex_index*3u;

    var vsOutput: OurVertexShaderOutput;

    var originalPosition = vec4f ( verts[i], verts[i+1], verts[i+2], 1.0);
    vsOutput.position = camera * model_matrix * originalPosition;

    vsOutput.texcoord = originalPosition.xy;
    vsOutput.normal = vec3f(normals[i], normals[i+1], normals[i+2]);

    return vsOutput;
}

@fragment
fn fs_main(fsInput: OurVertexShaderOutput) -> @location(0) vec4f {

    var color = textureSample(ourTexture, ourSampler, fsInput.texcoord);

    var lightFactor = 0.5 + 0.5*dot(fsInput.normal, normalize(vec3f(1.0, 1.0, -1.0f)));

    return vec4f(color.rgb * lightFactor, color.a);
}
