sealed trait Option[+A] {
    /** Transforme une `Option[A]` en `Option[B]` via `f`. */
    def map[B](f:A => B):Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a)) 
    }
    /** `get` si c'est un `Some[A]` sinon évaluation de `default`. */
    def getOrElse[B >: A](default: => B):B = this match {
        case None => default
        case Some(a) => a 
    }
    /** `f` appelé sur la valeur de cette option si non vide. */
    def flatMap[B](f:A => Option[B]):Option[B] = this match {
        case None => None
        case Some(a) => f(a)  
    }
    /** Évalue `default` si vide, sinon retourne cette option. */
    def orElse[B >: A](default: => Option[B]):Option[B] = this match
    {
        case None => default
        case _ => this
    }
    /** Cette option si le prédicat `f` vaut false. */
    def filter(f:A => Boolean):Option[A] = this match {
        case Some(a) if(f(a)) => this  
        case None => None    
    }
}
case object None extends Option[Nothing]
case class Some[+A](get:A) extends Option[A]

object TestOption {
    def main(args: Array[String]): Unit = {
        val a = Some(1)
        val b = Some(2)
        val c  : Option[Int] = None
    }
}


