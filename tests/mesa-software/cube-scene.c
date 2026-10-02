#include "util/glheader.h"
#include "glapi/glapi/glapi.h"
#include "cube-scene.h"
#include <string.h>
#include <stddef.h>
#include <cubit/debug.h>
#define LOAD(result, name, arguments) \
    typedef result (*name##_fn) arguments; \
    name##_fn name = (name##_fn)_mesa_glapi_get_proc_address(#name); \
    if (!name) return 0

/* Test scene has exactly one context, retained until VM termination. */
static GLuint cube_program;
static GLuint cube_buffers[2];
static GLuint cube_texture;
static void shader_log(const char *text)
{
    cubit_debug_write(text, strlen(text));
}
static int prepare_program(void)
{
    LOAD(GLuint, glCreateShader, (GLenum));
    LOAD(void, glShaderSource, (GLuint, GLsizei, const GLchar *const *, const GLint *));
    LOAD(void, glCompileShader, (GLuint));
    LOAD(void, glGetShaderiv, (GLuint, GLenum, GLint *));
    LOAD(void, glGetShaderInfoLog, (GLuint, GLsizei, GLsizei *, GLchar *));
    LOAD(void, glDeleteShader, (GLuint));
    LOAD(GLuint, glCreateProgram, (void));
    LOAD(void, glAttachShader, (GLuint, GLuint));
    LOAD(void, glBindAttribLocation, (GLuint, GLuint, const GLchar *));
    LOAD(void, glLinkProgram, (GLuint));
    LOAD(void, glGetProgramiv, (GLuint, GLenum, GLint *));
    LOAD(void, glGetProgramInfoLog, (GLuint, GLsizei, GLsizei *, GLchar *));
    LOAD(void, glDeleteProgram, (GLuint));
    const GLchar *sources[2] = {
        "#version 120\nattribute vec3 position; attribute vec3 color;\n"
        "attribute vec2 texcoord; varying vec2 uv; varying vec4 cube_color;\n"
        "void main() { gl_Position = gl_ModelViewProjectionMatrix * vec4(position, 1.0);"
        " cube_color = vec4(color, 1.0); uv = texcoord; }\n",
        "#version 120\nvarying vec4 cube_color; varying vec2 uv; uniform sampler2D tiles;\n"
        "void main() { gl_FragColor = cube_color * texture2D(tiles, uv); }\n",
    };
    GLuint shaders[2] = {0, 0}, program = 0;
    GLint ok = GL_FALSE;
    char log[1024] = {0};
    for (unsigned i = 0; i < 2; ++i) {
        shaders[i] = glCreateShader(i ? GL_FRAGMENT_SHADER : GL_VERTEX_SHADER);
        if (!shaders[i]) goto fail;
        glShaderSource(shaders[i], 1, &sources[i], NULL);
        glCompileShader(shaders[i]);
        glGetShaderiv(shaders[i], GL_COMPILE_STATUS, &ok);
        if (!ok) {
            glGetShaderInfoLog(shaders[i], sizeof(log), NULL, log);
            shader_log("MESA-WINDOW: FAIL shader compilation: ");
            shader_log(log); shader_log("\n");
            goto fail;
        }
    }
    program = glCreateProgram();
    if (!program) goto fail;
    for (unsigned i = 0; i < 2; ++i) glAttachShader(program, shaders[i]);
    glBindAttribLocation(program, 0, "position");
    glBindAttribLocation(program, 1, "color");
    glBindAttribLocation(program, 2, "texcoord");
    glLinkProgram(program);
    glGetProgramiv(program, GL_LINK_STATUS, &ok);
    if (!ok) {
        glGetProgramInfoLog(program, sizeof(log), NULL, log);
        shader_log("MESA-WINDOW: FAIL shader link: ");
        shader_log(log); shader_log("\n");
        goto fail;
    }
    for (unsigned i = 0; i < 2; ++i) glDeleteShader(shaders[i]);
    cube_program = program;
    shader_log("MESA-WINDOW: GLSL vertex/fragment compile and link PASS\n");
    return 1;
fail:
    if (program) glDeleteProgram(program);
    for (unsigned i = 0; i < 2; ++i)
        if (shaders[i]) glDeleteShader(shaders[i]);
    return 0;
}

int cubit_draw_cube(unsigned frame, unsigned width, unsigned height)
{
    LOAD(void, glUseProgram, (GLuint));
    if (!cube_program && !prepare_program()) return 0;
    glUseProgram(cube_program);
    LOAD(void, glEnable, (GLenum)); LOAD(void, glDisable, (GLenum));
    LOAD(void, glDepthFunc, (GLenum)); LOAD(void, glDepthMask, (GLboolean));
    LOAD(void, glClearDepth, (GLdouble)); LOAD(void, glClear, (GLbitfield));
    LOAD(void, glClearColor, (GLfloat, GLfloat, GLfloat, GLfloat));
    LOAD(void, glMatrixMode, (GLenum)); LOAD(void, glLoadIdentity, (void));
    LOAD(void, glOrtho, (GLdouble, GLdouble, GLdouble, GLdouble, GLdouble, GLdouble));
    LOAD(void, glTranslatef, (GLfloat, GLfloat, GLfloat));
    LOAD(void, glRotatef, (GLfloat, GLfloat, GLfloat, GLfloat));
    LOAD(void, glGenBuffers, (GLsizei, GLuint *));
    LOAD(void, glBindBuffer, (GLenum, GLuint));
    LOAD(void, glBufferData, (GLenum, GLsizeiptr, const void *, GLenum));
    LOAD(void, glGetBufferParameteriv, (GLenum, GLenum, GLint *));
    LOAD(GLenum, glGetError, (void));
    LOAD(void, glEnableVertexAttribArray, (GLuint));
    LOAD(void, glVertexAttribPointer, (GLuint, GLint, GLenum, GLboolean, GLsizei, const void *));
    LOAD(void, glDrawElements, (GLenum, GLsizei, GLenum, const void *));
    LOAD(void, glGenTextures, (GLsizei, GLuint *));
    LOAD(void, glActiveTexture, (GLenum));
    LOAD(void, glBindTexture, (GLenum, GLuint));
    LOAD(void, glTexParameteri, (GLenum, GLenum, GLint));
    LOAD(void, glTexImage2D, (GLenum, GLint, GLint, GLsizei, GLsizei, GLint, GLenum, GLenum, const void *));
    LOAD(GLint, glGetUniformLocation, (GLuint, const GLchar *));
    LOAD(void, glUniform1i, (GLint, GLint));
    LOAD(void, glScissor, (GLint, GLint, GLsizei, GLsizei));
    static const GLfloat vertices[8][3] = {
        {-.6f,-.6f,-.6f}, {.6f,-.6f,-.6f}, {-.6f,.6f,-.6f}, {.6f,.6f,-.6f},
        {-.6f,-.6f,.6f}, {.6f,-.6f,.6f}, {-.6f,.6f,.6f}, {.6f,.6f,.6f},
    };
    static const unsigned faces[6][4] = {
        {0,2,6,4}, {1,5,7,3}, {0,4,5,1}, {2,3,7,6}, {0,1,3,2}, {4,6,7,5},
    };
    static const GLfloat colors[6][3] = {
        {1,0,0}, {0,1,0}, {0,0,1}, {1,1,0}, {1,0,1}, {0,1,1},
    };
    struct vertex { GLfloat position[3], color[3], uv[2]; };
    if (!cube_buffers[0]) {
        struct vertex data[24];
        GLushort indices[36];
        const unsigned order[6] = {0, 1, 2, 0, 2, 3};
        const GLfloat uv[4][2] = {{0,0}, {1,0}, {1,1}, {0,1}};
        for (unsigned face = 0; face < 6; ++face) {
            for (unsigned j = 0; j < 4; ++j) {
                memcpy(data[face * 4 + j].position, vertices[faces[face][j]], sizeof(data[0].position));
                memcpy(data[face * 4 + j].color, colors[face], sizeof(data[0].color));
                memcpy(data[face * 4 + j].uv, uv[j], sizeof(data[0].uv));
            }
            for (unsigned j = 0; j < 6; ++j)
                indices[face * 6 + j] = face * 4 + order[j];
        }
        glGenBuffers(2, cube_buffers);
        if (!cube_buffers[0] || !cube_buffers[1]) return 0;
        glBindBuffer(GL_ARRAY_BUFFER, cube_buffers[0]);
        glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_buffers[1]);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(indices), indices, GL_STATIC_DRAW);
        GLint vertex_bytes = 0, index_bytes = 0;
        glGetBufferParameteriv(GL_ARRAY_BUFFER, GL_BUFFER_SIZE, &vertex_bytes);
        glGetBufferParameteriv(GL_ELEMENT_ARRAY_BUFFER, GL_BUFFER_SIZE, &index_bytes);
        if (glGetError() != GL_NO_ERROR || vertex_bytes != sizeof(data) ||
            index_bytes != sizeof(indices)) return 0;
        shader_log("MESA-WINDOW: vertex/index buffers uploaded PASS\n");
    }
    glActiveTexture(GL_TEXTURE0);
    if (!cube_texture) {
        GLubyte texels[4][4][4];
        for (unsigned y = 0; y < 4; ++y)
            for (unsigned x = 0; x < 4; ++x) {
                /* Asymmetric levels detect UV swaps, flips and constant samples. */
                for (unsigned c = 0; c < 3; ++c) texels[y][x][c] = 64 + 32*x + 8*y;
                texels[y][x][3] = 255;
            }
        glGenTextures(1, &cube_texture);
        if (!cube_texture) return 0;
        glBindTexture(GL_TEXTURE_2D, cube_texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 4, 4, 0, GL_RGBA, GL_UNSIGNED_BYTE, texels);
        GLint sampler = glGetUniformLocation(cube_program, "tiles");
        if (sampler < 0) return 0;
        glUniform1i(sampler, 0);
        if (glGetError() != GL_NO_ERROR) return 0;
        shader_log("MESA-WINDOW: RGBA texture uploaded PASS\n");
    }
    glBindTexture(GL_TEXTURE_2D, cube_texture);
    glBindBuffer(GL_ARRAY_BUFFER, cube_buffers[0]);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, cube_buffers[1]);
    glEnableVertexAttribArray(0); glEnableVertexAttribArray(1);
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(struct vertex),
                          (const void *)offsetof(struct vertex, position));
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, sizeof(struct vertex),
                          (const void *)offsetof(struct vertex, color));
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, sizeof(struct vertex),
                          (const void *)offsetof(struct vertex, uv));
    glEnable(GL_DEPTH_TEST); glDepthFunc(GL_LESS); glDepthMask(GL_TRUE);
    glClearDepth(1); glClearColor(.125f,.125f,.125f,1);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glMatrixMode(GL_PROJECTION); glLoadIdentity();
    glOrtho(-1.6,1.6,-1.2,1.2,1,10);
    glMatrixMode(GL_MODELVIEW); glLoadIdentity();
    glTranslatef(0,0,-4); glRotatef(25,1,0,0); glRotatef(35 + frame * 10,0,1,0);
    glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_SHORT, NULL);
    /* Small deterministic origin marker for the independent screenshot oracle. */
    glEnable(GL_SCISSOR_TEST); glScissor(0, height - 8, 8, 8);
    glClearColor(1,0,0,1); glClear(GL_COLOR_BUFFER_BIT); glDisable(GL_SCISSOR_TEST);
    (void)width;
    return 1;
}
