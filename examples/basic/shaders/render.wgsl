
struct Uniforms {
matrix: mat4x4f,
};

@group(0) @binding(0) var<uniform> uni: Uniforms;
@group(0) @binding(1) var<storage, read> v1: array<f32>;

@group(0) @binding(2) var ourTexture: texture_2d<f32>;
@group(0) @binding(3) var ourSampler: sampler;

struct OurVertexShaderOutput {
    @builtin(position) position: vec4f,
    @location(0) texcoord: vec2f,
};

@vertex
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> OurVertexShaderOutput {


// weird vec3f has 16 byte alignment, not 12!
//   return vec4f(v1[in_vertex_index].xyz, 1.0);
    var i = in_vertex_index*3u;

    var vsOutput: OurVertexShaderOutput;
    var originalPosition = vec4f ( v1[i], v1[i+1], v1[i+2], 1.0);
    vsOutput.position = uni.matrix* originalPosition;
//vsOutput.position.z = 0.5;
//vsOutput.position.w = 1.0;

    vsOutput.texcoord = originalPosition.xy;
    return vsOutput;
}

@fragment
fn fs_main(fsInput: OurVertexShaderOutput) -> @location(0) vec4f {

    // return vec4f(1.0, 0.0, 0.0, 1.0);
    var edge = 0.08;
    var coord = vec2f(fsInput.texcoord.x,1.0 - fsInput.texcoord.y);
    coord.x = edge/2.0 + (1.0 - edge)*coord.x;
    coord.y = edge/2.0 + (1.0 - edge)*coord.y;
    var color = textureSample(ourTexture, ourSampler, coord);
 var oldr = color.r;
 var oldg = color.g;
 var oldb = color.b;
 var olda = color.a;

 // convert from BufferedImage/TYPE_4BYTE_ABGR to raw/WGPUTextureFormat_RGBA8Unorm
 color.r = olda;
 color.b = oldg;
 color.g = oldb;
 color.a = oldr;

 return color;





}
