package script

import scala.collection.mutable.{HashMap,Stack}

class Env (val parent:Env) {
  var e:HashMap[Any, Any] = new scala.collection.mutable.HashMap
  def apply(a:Any):Any = {
    if (e.contains(a)) e(a)
    else if(parent != null)parent(a)
    else null
  }
  def contains(a:Any) : Boolean = {
    if(e.contains(a)) true
    else if(parent == null) false
    else parent.contains(a)
  }
  def +=(kv : (Any, Any)) : Env = {
    def add(env:Env, kv:(Any, Any)) : Boolean = {
      kv match {
        case (a, b) =>
          if(env.e.contains(a)) { env.e += kv; true }
          else (env.parent != null && add(env.parent, kv))
      }
    }
    if(!add(this, kv)) {
      e += kv
    }
    this
  }
  override def toString():String = {
    e.toString()+"\nparent:"+parent
  }
}
