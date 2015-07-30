import org.lwjgl.Sys
import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.{GLFWKeyCallback, Callbacks}
import org.lwjgl.opengl.GLContext
import org.lwjgl.opengl.GL11._

object Main extends App {
  println("Hello basic-project!")
  println(s"LWJGL Version ${Sys.getVersion} is working.")

  glfwSetErrorCallback(Callbacks.errorCallbackPrint())
  if(glfwInit() != GL_TRUE){
    throw new IllegalStateException("Unable to initialize GLFW")
  }

  val window = glfwCreateWindow(640, 480, "Simple example", 0, 0);
  if (window == 0) {
    glfwTerminate();
    throw new RuntimeException("Failed to create the GLFW window");
  }

  glfwMakeContextCurrent(window)
  GLContext.createFromCurrent()

  glfwSetKeyCallback(window, new GLFWKeyCallback {
    override def invoke(winsow: Long, key: Int, i1: Int, action: Int, i3: Int): Unit = if(key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) glfwSetWindowShouldClose(window, GL_TRUE)
  })

  while (glfwWindowShouldClose(window) != GL_TRUE) {
    val time = glfwGetTime()
    glfwSwapBuffers(window)
    glfwPollEvents()
  }
}
