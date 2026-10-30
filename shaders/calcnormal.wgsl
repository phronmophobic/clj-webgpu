
@group(0) @binding(0)
var<storage, read> triVerts : array<i32>;
@group(0) @binding(1)
var<storage, read> pos : array<f32>;
@group(0) @binding(2)
var<storage, read> length : u32;

@group(0) @binding(3)
var<storage, read_write> normals : array<f32>;


@compute @workgroup_size(256)
fn main(@builtin(global_invocation_id) gid : vec3<u32>) {
  let triIndex = gid.x;
  if (triIndex >= length) {
    return;
  }

    let cornerIndex1 = triVerts[triIndex*3 + 0];
    let cornerIndex2 = triVerts[triIndex*3 + 1];
    let cornerIndex3 = triVerts[triIndex*3 + 2];

    let corner1 = vec3f(pos[cornerIndex1*3 + 0], pos[cornerIndex1*3 + 1], pos[cornerIndex1*3 + 2]);
    let corner2 = vec3f(pos[cornerIndex2*3 + 0], pos[cornerIndex2*3 + 1], pos[cornerIndex2*3 + 2]);
    let corner3 = vec3f(pos[cornerIndex3*3 + 0], pos[cornerIndex3*3 + 1], pos[cornerIndex3*3 + 2]);

    let edge1 = corner2 - corner1;
    let edge2 = corner3 - corner1;

    let normal = normalize(cross(edge1, edge2));

    // use same normal for all 
    normals[triIndex*3 + 0] = normal.x;
    normals[triIndex*3 + 1] = normal.y;
    normals[triIndex*3 + 2] = normal.z;
}  

