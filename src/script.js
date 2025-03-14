import {
  Renderer,
  Program,
  Mesh,
  Transform,
  Geometry,
  Color,
  Post,
  Vec2,
  Texture,
  Text,
  RenderTarget
} from "https://cdn.skypack.dev/ogl";

const COLORS = [
  "#4ECDC4",
  "#FAD0C4",
  "#FFD1FF",
  "#B5EAD7",
  "#C7CEEA",
  "#F7A442",
  "#E942F7",
  "#0025EE",
  "#E9211C"
];

const circleFragment = /* glsl */ `
precision highp float;

uniform vec2 uResolution;

varying vec3 vColor;
varying vec2 vUv;

void main() {
    vec2 uv = gl_FragCoord.xy / uResolution.xy * 2.0 - 1.0;

    vec2 centeredUv = vUv - vec2(0.5, 0.5);

    float radius = 0.5;
    float dist = length(centeredUv);

    if (dist > radius) {
        discard;
    }

    float circle = 1.0 - smoothstep(radius, radius, dist);

    vec4 color = vec4(vColor, circle);

    gl_FragColor = color;
}`;

const circleVertex = /* glsl */ `
precision highp float;

attribute vec2 position;
attribute vec2 uv;
attribute vec2 translation;
attribute vec3 color;
attribute float radius;

uniform float uAspect;

varying vec2 vUv;
varying vec3 vColor;

void main() {
    vUv = uv;

    vColor = color;

    vec2 scaledPosition = position * radius;
    // scaledPosition.x *= uAspect;
    gl_Position = vec4(scaledPosition + translation, 0, 1);
}`;

const blurFragment = /* glsl */ `
precision highp float;

vec4 blur13(sampler2D image, vec2 uv, vec2 resolution, vec2 direction) {
    vec4 color = vec4(0.0);
    vec2 off1 = vec2(1.411764705882353) * direction;
    vec2 off2 = vec2(3.2941176470588234) * direction;
    vec2 off3 = vec2(5.176470588235294) * direction;
    color += texture2D(image, uv) * 0.1964825501511404;
    color += texture2D(image, uv + (off1 / resolution)) * 0.2969069646728344;
    color += texture2D(image, uv - (off1 / resolution)) * 0.2969069646728344;
    color += texture2D(image, uv + (off2 / resolution)) * 0.09447039785044732;
    color += texture2D(image, uv - (off2 / resolution)) * 0.09447039785044732;
    color += texture2D(image, uv + (off3 / resolution)) * 0.010381362401148057;
    color += texture2D(image, uv - (off3 / resolution)) * 0.010381362401148057;
    return color;
}

uniform sampler2D tMap;
uniform vec2 uDirection;
uniform vec2 uResolution;

varying vec2 vUv;

void main() {
    gl_FragColor = blur13(tMap, vUv, uResolution, uDirection);
}
`;

const compositeFragment = /* glsl */ `
precision highp float;

uniform sampler2D tMap;
uniform sampler2D tBloom;
uniform sampler2D tFluid;
uniform vec2 uResolution;
uniform float uTime;
uniform vec3 uBg;
uniform bool uTop;
uniform bool uBottom;

varying vec2 vUv;

void main() {
    vec4 fluid = texture2D(tFluid, vUv);

    vec2 uv = vUv;
    uv = uv * 2.0 - 1.0; // [-1,1] uv range.

    float chromaticOffset = 0.002; // Adjust for stronger or weaker effect
    vec2 offsetR = fluid.rr * chromaticOffset * -0.5;
    vec2 offsetG = fluid.gg * chromaticOffset * -0.5;
    vec2 offsetB = fluid.bb * chromaticOffset * -0.5;

    float r = texture2D(tBloom, vUv - fluid.bb * 0.0015).r;
    float g = texture2D(tBloom, vUv - fluid.gg * 0.0030).g;
    float b = texture2D(tBloom, vUv - fluid.rr * 0.0035).b;

    float mr = texture2D(tMap, vUv - fluid.rg * 0.0002).r;
    float mg = texture2D(tMap, vUv - fluid.rg * 0.0003).g;
    float mb = texture2D(tMap, vUv - fluid.rg * 0.0004).b;

    vec4 c = vec4(r, g, b, 1.0) + (1. - vec4(mr, mg, mb, 0.));

    float noiseAmount = 0.1;
    float n = fract(sin(dot(vUv, vec2(uTime + 12.9898, 78.233))) * 43758.5453);
    c *= (1.0 - noiseAmount + n * noiseAmount);

    if (!uTop && !uBottom) {
      gl_FragColor = c;
      return;
    }

    vec2 st = (gl_FragCoord.xy - 0.5 * uResolution.xy) / uResolution.y + 0.5;
    float time = uTime * 0.5;

    float col;

    // Bottom wave effect
    if (uBottom) {
        vec3 s = cos(3.5 * st.x * vec3(1.0, 2.0, 3.0) - time * vec3(1.0, -1.0, 1.0)) * 0.3;
        float cutBottom = st.y + 0.01 + (s.x + s.y + s.z) / 50.0;
        col = smoothstep(0.0, 0.2, cutBottom - 0.04);
    }

    if (uTop) {
        vec3 s = cos(3.5 * st.x * vec3(1.0, 2.0, 3.0) - time * vec3(-1.0, -1.0, 1.0)) * 0.25;
        float cutTop = (0.501 - st.y) + (s.x + s.y + s.z) / 40.0;
        float c = smoothstep(0.0, 0.2, cutTop - 0.04);
        col = uBottom ? min(col, c) : c;
    }

    c = mix(vec4(uBg, 1.), c, col);

    gl_FragColor = c;
}
`;

const baseVertex = /* glsl */ `
    precision highp float;
    attribute vec2 position;
    attribute vec2 uv;
    varying vec2 vUv;
    varying vec2 vL;
    varying vec2 vR;
    varying vec2 vT;
    varying vec2 vB;
    uniform vec2 texelSize;
    void main () {
        vUv = uv;
        vL = vUv - vec2(texelSize.x, 0.0);
        vR = vUv + vec2(texelSize.x, 0.0);
        vT = vUv + vec2(0.0, texelSize.y);
        vB = vUv - vec2(0.0, texelSize.y);
        gl_Position = vec4(position, 0, 1);
    }
`;

const clearShader = /* glsl */ `
    precision mediump float;
    precision mediump sampler2D;
    varying highp vec2 vUv;
    uniform sampler2D uTexture;
    uniform float value;
    void main () {
        gl_FragColor = value * texture2D(uTexture, vUv);
    }
`;

const splatShader = /* glsl */ `
    precision highp float;
    precision highp sampler2D;
    varying vec2 vUv;
    uniform sampler2D uTarget;
    uniform float aspectRatio;
    uniform vec3 color;
    uniform vec2 point;
    uniform float radius;
    void main () {
        vec2 p = vUv - point.xy;
        p.x *= aspectRatio;
        vec3 splat = exp(-dot(p, p) / radius) * color;
        vec3 base = texture2D(uTarget, vUv).xyz;
        gl_FragColor = vec4(base + splat, 1.0);
    }
`;

const advectionManualFilteringShader = /* glsl */ `
    precision highp float;
    precision highp sampler2D;
    varying vec2 vUv;
    uniform sampler2D uVelocity;
    uniform sampler2D uSource;
    uniform vec2 texelSize;
    uniform vec2 dyeTexelSize;
    uniform float dt;
    uniform float dissipation;
    vec4 bilerp (sampler2D sam, vec2 uv, vec2 tsize) {
        vec2 st = uv / tsize - 0.5;
        vec2 iuv = floor(st);
        vec2 fuv = fract(st);
        vec4 a = texture2D(sam, (iuv + vec2(0.5, 0.5)) * tsize);
        vec4 b = texture2D(sam, (iuv + vec2(1.5, 0.5)) * tsize);
        vec4 c = texture2D(sam, (iuv + vec2(0.5, 1.5)) * tsize);
        vec4 d = texture2D(sam, (iuv + vec2(1.5, 1.5)) * tsize);
        return mix(mix(a, b, fuv.x), mix(c, d, fuv.x), fuv.y);
    }
    void main () {
        vec2 coord = vUv - dt * bilerp(uVelocity, vUv, texelSize).xy * texelSize;
        gl_FragColor = dissipation * bilerp(uSource, coord, dyeTexelSize);
        gl_FragColor.a = 1.0;
    }
`;

const advectionShader = /* glsl */ `
    precision highp float;
    precision highp sampler2D;
    varying vec2 vUv;
    uniform sampler2D uVelocity;
    uniform sampler2D uSource;
    uniform vec2 texelSize;
    uniform float dt;
    uniform float dissipation;
    void main () {
        vec2 coord = vUv - dt * texture2D(uVelocity, vUv).xy * texelSize;
        gl_FragColor = dissipation * texture2D(uSource, coord);
        gl_FragColor.a = 1.0;
    }
`;

const divergenceShader = /* glsl */ `
    precision mediump float;
    precision mediump sampler2D;
    varying highp vec2 vUv;
    varying highp vec2 vL;
    varying highp vec2 vR;
    varying highp vec2 vT;
    varying highp vec2 vB;
    uniform sampler2D uVelocity;
    void main () {
        float L = texture2D(uVelocity, vL).x;
        float R = texture2D(uVelocity, vR).x;
        float T = texture2D(uVelocity, vT).y;
        float B = texture2D(uVelocity, vB).y;
        vec2 C = texture2D(uVelocity, vUv).xy;
        if (vL.x < 0.0) { L = -C.x; }
        if (vR.x > 1.0) { R = -C.x; }
        if (vT.y > 1.0) { T = -C.y; }
        if (vB.y < 0.0) { B = -C.y; }
        float div = 0.5 * (R - L + T - B);
        gl_FragColor = vec4(div, 0.0, 0.0, 1.0);
    }
`;

const curlShader = /* glsl */ `
    precision mediump float;
    precision mediump sampler2D;
    varying highp vec2 vUv;
    varying highp vec2 vL;
    varying highp vec2 vR;
    varying highp vec2 vT;
    varying highp vec2 vB;
    uniform sampler2D uVelocity;
    void main () {
        float L = texture2D(uVelocity, vL).y;
        float R = texture2D(uVelocity, vR).y;
        float T = texture2D(uVelocity, vT).x;
        float B = texture2D(uVelocity, vB).x;
        float vorticity = R - L - T + B;
        gl_FragColor = vec4(0.5 * vorticity, 0.0, 0.0, 1.0);
    }
`;

const vorticityShader = /* glsl */ `
    precision highp float;
    precision highp sampler2D;
    varying vec2 vUv;
    varying vec2 vL;
    varying vec2 vR;
    varying vec2 vT;
    varying vec2 vB;
    uniform sampler2D uVelocity;
    uniform sampler2D uCurl;
    uniform float curl;
    uniform float dt;
    void main () {
        float L = texture2D(uCurl, vL).x;
        float R = texture2D(uCurl, vR).x;
        float T = texture2D(uCurl, vT).x;
        float B = texture2D(uCurl, vB).x;
        float C = texture2D(uCurl, vUv).x;
        vec2 force = 0.5 * vec2(abs(T) - abs(B), abs(R) - abs(L));
        force /= length(force) + 0.0001;
        force *= curl * C;
        force.y *= -1.0;
        vec2 vel = texture2D(uVelocity, vUv).xy;
        gl_FragColor = vec4(vel + force * dt, 0.0, 1.0);
    }
`;

const pressureShader = /* glsl */ `
    precision mediump float;
    precision mediump sampler2D;
    varying highp vec2 vUv;
    varying highp vec2 vL;
    varying highp vec2 vR;
    varying highp vec2 vT;
    varying highp vec2 vB;
    uniform sampler2D uPressure;
    uniform sampler2D uDivergence;
    void main () {
        float L = texture2D(uPressure, vL).x;
        float R = texture2D(uPressure, vR).x;
        float T = texture2D(uPressure, vT).x;
        float B = texture2D(uPressure, vB).x;
        float C = texture2D(uPressure, vUv).x;
        float divergence = texture2D(uDivergence, vUv).x;
        float pressure = (L + R + B + T - divergence) * 0.25;
        gl_FragColor = vec4(pressure, 0.0, 0.0, 1.0);
    }
`;

const gradientSubtractShader = /* glsl */ `
    precision mediump float;
    precision mediump sampler2D;
    varying highp vec2 vUv;
    varying highp vec2 vL;
    varying highp vec2 vR;
    varying highp vec2 vT;
    varying highp vec2 vB;
    uniform sampler2D uPressure;
    uniform sampler2D uVelocity;
    void main () {
        float L = texture2D(uPressure, vL).x;
        float R = texture2D(uPressure, vR).x;
        float T = texture2D(uPressure, vT).x;
        float B = texture2D(uPressure, vB).x;
        vec2 velocity = texture2D(uVelocity, vUv).xy;
        velocity.xy -= vec2(R - L, T - B);
        gl_FragColor = vec4(velocity, 0.0, 1.0);
    }
`;

const textVertex = /* glsl */ `#version 300 es
    #define attribute in
    #define varying out

    uniform float uAspect;
    attribute vec2 position;
    attribute vec2 uv;
    varying vec2 vUv;

    void main() {
        vUv = uv;
        vec2 pos = position;
        pos.x *= uAspect;
        pos.y += 0.25;
        gl_Position = vec4(pos, 0., 1.);
    }
`;

const textFragment = /* glsl */ `#version 300 es
precision highp float;
#define varying in
#define texture2D texture
#define gl_FragColor FragColor
out vec4 FragColor;

uniform sampler2D tMap;

varying vec2 vUv;

void main() {
    vec3 tex = texture2D(tMap, vUv).rgb;
    float signedDist = max(min(tex.r, tex.g), min(max(tex.r, tex.g), tex.b)) - 0.5;
    float d = fwidth(signedDist);
    float alpha = smoothstep(-d, d, signedDist);

    if (alpha < 0.01) discard;

    gl_FragColor.a = alpha;
}
`;

class Bg extends HTMLElement {
  minRadius = 150;
  maxRadius = 350;
  velocityDissipation = 0.98;
  densityDissipation = 0.97;
  simRes = 128;
  dyeRes = 512;

  width = 0;
  height = 0;
  blurRatio = 0;
  topCurve = false;
  bottomCurve = false;
  colors = [];
  inited = false;

  renderer;
  ctx;
  scene;
  lastTime = performance.now() / 1000;
  time = 0;
  resolution = { value: new Vec2(0, 0) };
  aspect = { value: 0 };
  bloomResolution = { value: new Vec2(0, 0) };
  circles = [];
  bg = new Color("#fff");
  postBloom;
  postComposite;
  compositePass;
  circlesTranslation;
  splats = [];
  lastMouse = new Vec2();

  constructor() {
    super();

    const topFade = this.dataset.topFade !== undefined;
    const bottomFade = this.dataset.bottomFade !== undefined;

    this.topCurve = topFade;
    this.bottomCurve = bottomFade;
    this.colors = COLORS;

    this.init();

    requestAnimationFrame(this.tick);

    window.addEventListener("resize", () => {
      this.onResize();
    });

    // Create handlers to get mouse position and velocity
    const isTouchCapable = "ontouchstart" in window;
    if (isTouchCapable) {
      window.addEventListener("touchstart", this.updateMouse, false);
      window.addEventListener("touchmove", this.updateMouse, false);
    } else {
      window.addEventListener("mousemove", this.updateMouse, false);
    }
  }

  init() {
    if (this.inited) {
      return;
    }

    this.inited = true;

    const canvas = document.createElement("canvas");

    this.append(canvas);

    this.renderer = new Renderer({
      canvas,
      alpha: true,
      antialias: false,
      premultipliedAlpha: true
    });

    const gl = this.renderer.gl;
    this.tx = gl;

    const bg = this.bg;

    gl.colorMask(true, true, true, true);
    gl.clearColor(bg.r, bg.g, bg.b, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    const scene = new Transform();

    const program = new Program(gl, {
      transparent: false,
      depthTest: false,
      vertex: circleVertex,
      fragment: circleFragment,
      uniforms: {
        uResolution: this.resolution,
        uAspect: this.aspect,
        time: { value: this.time }
      }
    });

    const instanceCount = 40;
    const translations = new Float32Array(instanceCount * 2); // Two values for x and y
    const radii = new Float32Array(instanceCount); // One value for radius per instance
    const colors = new Float32Array(instanceCount * 3); // RGB color for each instance
    const circles = [];

    for (let i = 0; i < instanceCount; i++) {
      translations[i * 2] = Math.random() * 2 - 1;
      translations[i * 2 + 1] = Math.random() * 2 - 1;
      radii[i] = Math.random() + 0.2;

      const color = new Color(this.colors[i % this.colors.length]);
      colors[i * 3] = color.r;
      colors[i * 3 + 1] = color.g;
      colors[i * 3 + 2] = color.b;

      circles.push({
        vX: (Math.random() - 0.5) * 0.003 || 0.005,
        vY: (Math.random() - 0.5) * 0.003 || -0.005
      });
    }

    const geometry = new Geometry(gl, {
      position: {
        size: 3,
        data: new Float32Array([
          -0.5,
          0.5,
          0,
          -0.5,
          -0.5,
          0,
          0.5,
          0.5,
          0,
          0.5,
          -0.5,
          0
        ])
      },
      uv: { size: 2, data: new Float32Array([0, 1, 1, 1, 0, 0, 1, 0]) },
      index: { data: new Uint16Array([0, 1, 2, 1, 3, 2]) },
      translation: { size: 2, instanced: true, data: translations },
      radius: { size: 1, instanced: true, data: radii },
      color: { size: 3, instanced: true, data: colors }
    });

    geometry.setInstancedCount(instanceCount);

    this.circles = circles;
    this.circlesTranslation = geometry.attributes.translation;

    const mesh = new Mesh(gl, { geometry, program });
    mesh.setParent(scene);

    const postComposite = new Post(gl);
    const postBloom = new Post(gl, { dpr: 0.5, targetOnly: true });

    const horizontalPass = postBloom.addPass({
      fragment: blurFragment,
      uniforms: {
        uResolution: this.bloomResolution,
        uDirection: { value: [8, 0] }
      }
    });

    const verticalPass = postBloom.addPass({
      fragment: blurFragment,
      uniforms: {
        uResolution: this.bloomResolution,
        uDirection: { value: [0, 8] }
      }
    });

    for (let i = 0; i < 15; i++) {
      postBloom.passes.push(horizontalPass, verticalPass);
    }

    this.compositePass = postComposite.addPass({
      fragment: compositeFragment,
      uniforms: {
        uResolution: this.resolution,
        uTime: { value: this.time },
        uBg: { value: bg },
        uTop: { value: this.topCurve },
        uBottom: { value: this.bottomCurve },
        tBloom: postBloom.uniform,
        tFluid: { value: null }
      }
    });

    this.postComposite = postComposite;
    this.postBloom = postBloom;

    this.scene = scene;

    const simRes = this.simRes;
    const dyeRes = this.dyeRes;

    // Main inputs to control look and feel of fluid
    const pressureDissipation = 0.8;
    const curlStrength = 20;
    const radius = 0.2;

    // Common uniform
    const texelSize = { value: new Vec2(1 / simRes) };

    // Get supported formats and types for FBOs
    let supportLinearFiltering =
      gl.renderer.extensions[
        `OES_texture_${gl.renderer.isWebgl2 ? `` : `half_`}float_linear`
      ];
    const halfFloat = gl.renderer.isWebgl2
      ? gl.HALF_FLOAT
      : gl.renderer.extensions["OES_texture_half_float"].HALF_FLOAT_OES;

    const filtering = supportLinearFiltering ? gl.LINEAR : gl.NEAREST;
    let rgba, rg, r;

    if (gl.renderer.isWebgl2) {
      rgba = getSupportedFormat(gl, gl.RGBA16F, gl.RGBA, halfFloat);
      rg = getSupportedFormat(gl, gl.RG16F, gl.RG, halfFloat);
      r = getSupportedFormat(gl, gl.R16F, gl.RED, halfFloat);
    } else {
      rgba = getSupportedFormat(gl, gl.RGBA, gl.RGBA, halfFloat);
      rg = rgba;
      r = rgba;
    }

    // Create fluid simulation FBOs
    const density = createDoubleFBO(gl, {
      width: dyeRes,
      height: dyeRes,
      type: halfFloat,
      format: rgba?.format,
      internalFormat: rgba?.internalFormat,
      minFilter: filtering,
      depth: false
    });

    this.density = density;

    const velocity = createDoubleFBO(gl, {
      width: simRes,
      height: simRes,
      type: halfFloat,
      format: rg?.format,
      internalFormat: rg?.internalFormat,
      minFilter: filtering,
      depth: false
    });

    this.velocity = velocity;

    const pressure = createDoubleFBO(gl, {
      width: simRes,
      height: simRes,
      type: halfFloat,
      format: r?.format,
      internalFormat: r?.internalFormat,
      minFilter: gl.NEAREST,
      depth: false
    });

    this.pressure = pressure;

    const divergence = new RenderTarget(gl, {
      width: simRes,
      height: simRes,
      type: halfFloat,
      format: r?.format,
      internalFormat: r?.internalFormat,
      minFilter: gl.NEAREST,
      depth: false
    });

    const curl = new RenderTarget(gl, {
      width: simRes,
      height: simRes,
      type: halfFloat,
      format: r?.format,
      internalFormat: r?.internalFormat,
      minFilter: gl.NEAREST,
      depth: false
    });
    this.curl = curl;

    // Geometry to be used for the simulation programs
    const triangle = new Geometry(gl, {
      position: { size: 2, data: new Float32Array([-1, -1, 3, -1, -1, 3]) },
      uv: { size: 2, data: new Float32Array([0, 0, 2, 0, 0, 2]) }
    });

    // Create fluid simulation programs
    const clearProgram = new Mesh(gl, {
      geometry: triangle,
      program: new Program(gl, {
        vertex: baseVertex,
        fragment: clearShader,
        uniforms: {
          texelSize,
          uTexture: { value: null },
          value: { value: pressureDissipation }
        },
        depthTest: false,
        depthWrite: false
      })
    });
    this.clearProgram = clearProgram;

    const splatProgram = new Mesh(gl, {
      geometry: triangle,
      program: new Program(gl, {
        vertex: baseVertex,
        fragment: splatShader,
        uniforms: {
          texelSize,
          uTarget: { value: null },
          aspectRatio: { value: 1 },
          color: { value: new Color() },
          point: { value: new Vec2() },
          radius: { value: radius / 100 }
        },
        depthTest: false,
        depthWrite: false
      })
    });

    this.splatProgram = splatProgram;

    const advectionProgram = new Mesh(gl, {
      geometry: triangle,
      program: new Program(gl, {
        vertex: baseVertex,
        fragment: supportLinearFiltering
          ? advectionShader
          : advectionManualFilteringShader,
        uniforms: {
          texelSize,
          dyeTexelSize: { value: new Vec2(1 / dyeRes) },
          uVelocity: { value: null },
          uSource: { value: null },
          dt: { value: 0.016 },
          dissipation: { value: 1 }
        },
        depthTest: false,
        depthWrite: false
      })
    });

    this.advectionProgram = advectionProgram;

    const divergenceProgram = new Mesh(gl, {
      geometry: triangle,
      program: new Program(gl, {
        vertex: baseVertex,
        fragment: divergenceShader,
        uniforms: {
          texelSize,
          uVelocity: { value: null }
        },
        depthTest: false,
        depthWrite: false
      })
    });

    this.divergenceProgram = divergenceProgram;
    this.divergence = divergence;

    const curlProgram = new Mesh(gl, {
      geometry: triangle,
      program: new Program(gl, {
        vertex: baseVertex,
        fragment: curlShader,
        uniforms: {
          texelSize,
          uVelocity: { value: null }
        },
        depthTest: false,
        depthWrite: false
      })
    });

    this.curlProgram = curlProgram;

    const vorticityProgram = new Mesh(gl, {
      geometry: triangle,
      program: new Program(gl, {
        vertex: baseVertex,
        fragment: vorticityShader,
        uniforms: {
          texelSize,
          uVelocity: { value: null },
          uCurl: { value: null },
          curl: { value: curlStrength },
          dt: { value: 0.016 }
        },
        depthTest: false,
        depthWrite: false
      })
    });

    this.vorticityProgram = vorticityProgram;

    const pressureProgram = new Mesh(gl, {
      geometry: triangle,
      program: new Program(gl, {
        vertex: baseVertex,
        fragment: pressureShader,
        uniforms: {
          texelSize,
          uPressure: { value: null },
          uDivergence: { value: null }
        },
        depthTest: false,
        depthWrite: false
      })
    });

    this.pressureProgram = pressureProgram;

    const gradientSubtractProgram = new Mesh(gl, {
      geometry: triangle,
      program: new Program(gl, {
        vertex: baseVertex,
        fragment: gradientSubtractShader,
        uniforms: {
          texelSize,
          uPressure: { value: null },
          uVelocity: { value: null }
        },
        depthTest: false,
        depthWrite: false
      })
    });

    this.gradientSubtractProgram = gradientSubtractProgram;

    const texture = new Texture(gl, {
      generateMipmaps: false
    });
    const img = new Image();
    img.onload = () => (texture.image = img);
    img.crossOrigin = "";
    img.src =
      "https://cdn.jsdelivr.net/gh/oframe/ogl@master/examples/assets/fonts/FiraSans-Bold.png";

    const textProgram = new Program(gl, {
      vertex: textVertex,
      fragment: textFragment,
      uniforms: {
        tMap: { value: texture },
        uResolution: this.resolution,
        uAspect: this.aspect
      },
      transparent: true,
      cullFace: false,
      depthWrite: false
    });

    const textScene = new Transform();
    this.textScene = textScene;

    loadText();
    async function loadText() {
      const font = await (
        await fetch(
          "https://cdn.jsdelivr.net/gh/oframe/ogl@master/examples/assets/fonts/FiraSans-Bold.json"
        )
      ).json();

      const text = new Text({
        font,
        text: "Happy Holi",
        align: "center",
        letterSpacing: -0.06,
        size: 0.3,
        lineHeight: 1.1
      });

      // Pass the generated buffers into a geometry
      const geometry = new Geometry(gl, {
        position: { size: 3, data: text.buffers.position },
        uv: { size: 2, data: text.buffers.uv },
        id: { size: 1, data: text.buffers.id },
        index: { data: text.buffers.index }
      });

      const mesh = new Mesh(gl, { geometry, program: textProgram });

      // Use the height value to position text vertically. Here it is centered.
      // mesh.position.y = text.height * 0.5;
      mesh.setParent(textScene);
    }

    this.onResize();
  }

  onResize = () => {
    const renderer = this.renderer;

    if (!renderer) {
      return;
    }

    const { width: _width, height: _height } = this.getBoundingClientRect();

    renderer.setSize(_width, _height);

    const width = renderer.width;
    const height = renderer.height;

    this.resolution.value.set(width, height);

    this.aspect.value = height / width;

    this.postBloom.resize();
    this.postComposite.resize();

    this.bloomResolution.value.set(
      this.postBloom.resolutionHeight,
      this.postBloom.resolutionHeight
    );
  };

  updateMouse = (e) => {
    const lastMouse = this.lastMouse;
    const renderer = this.renderer;

    if (e.changedTouches && e.changedTouches.length) {
      e.x = e.changedTouches[0].pageX;
      e.y = e.changedTouches[0].pageY;
    }
    if (e.x === undefined) {
      e.x = e.pageX;
      e.y = e.pageY;
    }

    if (!lastMouse.isInit) {
      lastMouse.isInit = true;

      // First input
      lastMouse.set(e.x, e.y);
    }

    const deltaX = e.x - lastMouse.x;
    const deltaY = e.y - lastMouse.y;

    lastMouse.set(e.x, e.y);

    // Add if the mouse is moving
    if (Math.abs(deltaX) || Math.abs(deltaY)) {
      this.splats.push({
        // Get mouse value in 0 to 1 range, with y flipped
        x: e.x / renderer.width,
        y: 1 - e.y / renderer.height,
        dx: deltaX * 5,
        dy: deltaY * -5
      });
    }
  };

  splat({ x, y, dx, dy }) {
    const splatProgram = this.splatProgram;
    const velocity = this.velocity;
    const renderer = this.renderer;
    const density = this.density;

    splatProgram.program.uniforms.uTarget.value = velocity.read.texture;
    splatProgram.program.uniforms.aspectRatio.value =
      renderer.width / renderer.height;
    splatProgram.program.uniforms.point.value.set(x, y);
    splatProgram.program.uniforms.color.value.set(dx, dy, 1);

    renderer.render({
      scene: splatProgram,
      target: velocity.write,
      sort: false,
      update: false
    });
    velocity.swap();

    splatProgram.program.uniforms.uTarget.value = density.read.texture;

    renderer.render({
      scene: splatProgram,
      target: density.write,
      sort: false,
      update: false
    });
    density.swap();
  }

  tick = () => {
    const now = performance.now() / 1000;
    this.time += now - this.lastTime;
    this.lastTime = now;

    const compositePass = this.compositePass;
    const postComposite = this.postComposite;
    const density = this.density;
    const postBloom = this.postBloom;
    const renderer = this.renderer;
    const splats = this.splats;
    const curlProgram = this.curlProgram;
    const curl = this.curl;
    const velocity = this.velocity;
    const vorticityProgram = this.vorticityProgram;
    const divergenceProgram = this.divergenceProgram;
    const clearProgram = this.clearProgram;
    const pressure = this.pressure;
    const divergence = this.divergence;
    const pressureProgram = this.pressureProgram;
    const gradientSubtractProgram = this.gradientSubtractProgram;
    const advectionProgram = this.advectionProgram;
    const simRes = this.simRes;
    const dyeRes = this.dyeRes;
    const scene = this.scene;
    const velocityDissipation = this.velocityDissipation;
    const densityDissipation = this.densityDissipation;
    const textScene = this.textScene;

    const attr = this.circlesTranslation;
    const data = attr.data;

    this.circles.forEach((c, i) => {
      const ut = [data[i * 2], data[i * 2 + 1]];

      ut[0] += c.vX;
      ut[1] += c.vY;

      // Bounce off the edges
      if (ut[0] < -1 || ut[0] > 1) c.vX *= -1;
      if (ut[1] < -1 || ut[1] > 1) c.vY *= -1;

      data[i * 2] = ut[0];
      data[i * 2 + 1] = ut[1];
    });

    attr.needsUpdate = true;

    // Perform all of the fluid simulation renders
    // No need to clear during sim, saving a number of GL calls.
    renderer.autoClear = false;

    // Render all of the inputs since last frame
    for (let i = splats.length - 1; i >= 0; i--) {
      this.splat(splats.splice(i, 1)[0]);
    }

    curlProgram.program.uniforms.uVelocity.value = velocity.read.texture;

    renderer.render({
      scene: curlProgram,
      target: curl,
      sort: false,
      update: false
    });

    vorticityProgram.program.uniforms.uVelocity.value = velocity.read.texture;
    vorticityProgram.program.uniforms.uCurl.value = curl.texture;

    renderer.render({
      scene: vorticityProgram,
      target: velocity.write,
      sort: false,
      update: false
    });
    velocity.swap();

    divergenceProgram.program.uniforms.uVelocity.value = velocity.read.texture;

    renderer.render({
      scene: divergenceProgram,
      target: divergence,
      sort: false,
      update: false
    });

    clearProgram.program.uniforms.uTexture.value = pressure.read.texture;

    renderer.render({
      scene: clearProgram,
      target: pressure.write,
      sort: false,
      update: false
    });
    pressure.swap();

    pressureProgram.program.uniforms.uDivergence.value = divergence.texture;

    for (let i = 0; i < 3; i++) {
      pressureProgram.program.uniforms.uPressure.value = pressure.read.texture;

      renderer.render({
        scene: pressureProgram,
        target: pressure.write,
        sort: false,
        update: false
      });
      pressure.swap();
    }

    gradientSubtractProgram.program.uniforms.uPressure.value =
      pressure.read.texture;
    gradientSubtractProgram.program.uniforms.uVelocity.value =
      velocity.read.texture;

    renderer.render({
      scene: gradientSubtractProgram,
      target: velocity.write,
      sort: false,
      update: false
    });
    velocity.swap();

    advectionProgram.program.uniforms.dyeTexelSize.value.set(1 / simRes);
    advectionProgram.program.uniforms.uVelocity.value = velocity.read.texture;
    advectionProgram.program.uniforms.uSource.value = velocity.read.texture;
    advectionProgram.program.uniforms.dissipation.value = velocityDissipation;

    renderer.render({
      scene: advectionProgram,
      target: velocity.write,
      sort: false,
      update: false
    });
    velocity.swap();

    advectionProgram.program.uniforms.dyeTexelSize.value.set(1 / dyeRes);
    advectionProgram.program.uniforms.uVelocity.value = velocity.read.texture;
    advectionProgram.program.uniforms.uSource.value = density.read.texture;
    advectionProgram.program.uniforms.dissipation.value = densityDissipation;

    renderer.render({
      scene: advectionProgram,
      target: density.write,
      sort: false,
      update: false
    });
    density.swap();

    renderer.autoClear = true;

    compositePass.uniforms.tFluid.value = density.read.texture;
    compositePass.uniforms.uTime.value = this.time;

    compositePass.enabled = false;
    postComposite.targetOnly = true;
    postComposite.render({ scene });
    // postComposite.autoClear = false;

    postBloom.render({ texture: postComposite.uniform.value });

    postComposite.targetOnly = false;
    compositePass.enabled = true;

    postComposite.render({
      scene: textScene
    });

    // postComposite.render({ texture: postComposite.uniform.value });

    requestAnimationFrame(this.tick);
  };
}

// Helper to create a ping-pong FBO pairing for simulating on GPU
function createDoubleFBO(
  gl,
  {
    width,
    height,
    wrapS,
    wrapT,
    minFilter = gl.LINEAR,
    magFilter = minFilter,
    type,
    format,
    internalFormat,
    depth
  } = {}
) {
  const options = {
    width,
    height,
    wrapS,
    wrapT,
    minFilter,
    magFilter,
    type,
    format,
    internalFormat,
    depth
  };
  const fbo = {
    read: new RenderTarget(gl, options),
    write: new RenderTarget(gl, options),
    swap: () => {
      let temp = fbo.read;
      fbo.read = fbo.write;
      fbo.write = temp;
    }
  };
  return fbo;
}

// Helper functions for larger device support
function getSupportedFormat(gl, internalFormat, format, type) {
  if (!supportRenderTextureFormat(gl, internalFormat, format, type)) {
    switch (internalFormat) {
      case gl.R16F:
        return getSupportedFormat(gl, gl.RG16F, gl.RG, type);
      case gl.RG16F:
        return getSupportedFormat(gl, gl.RGBA16F, gl.RGBA, type);
      default:
        return null;
    }
  }

  return { internalFormat, format };
}

function supportRenderTextureFormat(gl, internalFormat, format, type) {
  let texture = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, texture);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  gl.texImage2D(gl.TEXTURE_2D, 0, internalFormat, 4, 4, 0, format, type, null);

  let fbo = gl.createFramebuffer();
  gl.bindFramebuffer(gl.FRAMEBUFFER, fbo);
  gl.framebufferTexture2D(
    gl.FRAMEBUFFER,
    gl.COLOR_ATTACHMENT0,
    gl.TEXTURE_2D,
    texture,
    0
  );

  const status = gl.checkFramebufferStatus(gl.FRAMEBUFFER);
  if (status != gl.FRAMEBUFFER_COMPLETE) return false;
  return true;
}

customElements.define("bg-grad", Bg);
