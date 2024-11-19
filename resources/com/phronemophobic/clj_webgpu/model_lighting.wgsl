
struct Uniforms {
matrix: mat4x4f,
};

@group(0) @binding(0) var<uniform> uni: Uniforms;
@group(0) @binding(1) var<storage, read> v1: array<f32>;
@group(0) @binding(2) var<storage, read> normals: array<f32>;

@group(0) @binding(3) var ourTexture: texture_2d<f32>;
@group(0) @binding(4) var ourSampler: sampler;

@group(0) @binding(5) var<uniform> uni2: Uniforms;

struct OurVertexShaderOutput {
    @builtin(position) position: vec4f,
    @location(0) texcoord: vec2f,
    @location(1) normal: vec3f,
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
    vsOutput.normal = (uni2.matrix* vec4f( normals[i], normals[i+1], normals[i+2], 1.0)).xyz;

    var a = uni2;

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

    if ( color.a < 0.2 ){
        var gray = 0.5 + 0.5*(fract(fsInput.position.x/10));
        color = vec4f(1.0 , 1.0 , 1.0 , 1.0);
    }

    let normal = normalize(fsInput.normal);
    var light = dot(normal, normalize(vec3f(0.0, 0.0f, 1.0f)));
    var lightRange = 0.9;
    light = 1.0;

    // return vec4f(1.0, 1.0, 1.0, 0.5);

    light = (1.0 - lightRange) + max(0.0, (lightRange * light));
    return vec4f(color.rgb * light, 0.2);
}
