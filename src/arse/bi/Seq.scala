package arse.bi


object Seq {
  def apply[A, S](l: Lit[S], p: Rel[A, S]) = new Rel[A, S]() {
    override def toString = l + " " + p

    def parse(s0: S) = {
      val s1 = l parse s0
      val (a, s2) = p parse s1
      (a, s2)
    }

    def format[B >: A](bs: (B, S)) = {
      val (a, s0) = bs
      val s1 = p format (a, s0)
      val s2 = l format s1
      s2
    }
  }
  
  def apply[A, S](p: Rel[A, S], l: Lit[S]) = new Rel[A, S]() {
    override def toString = p + " " + l

    def parse(s0: S) = {
      val (a, s1) = p parse s0
      val s2 = l parse s1
      (a, s2)
    }

    def format[B >: A](bs: (B, S)) = {
      val (a, s0) = bs
      val s1 = l format s0
      val s2 = p format (a, s1)
      s2
    }
  }
  
  def apply[A1, A2, S](p1: Rel[A1, S], p2: Rel[A2, S]) = new Rel[(A1, A2), S]() {
    override def toString = "(" + p1 + ", " + p2 + ")"

    def parse(s0: S) = {
      val (a1, s1) = p1 parse s0
      val (a2, s2) = p2 parse s1
      ((a1, a2), s2)
    }

    def format[B >: (A1, A2)](bs: (B, S)) = {
      val ((a1, a2), s0) = bs
      val s1 = p2 format (a2, s0)
      val s2 = p1 format (a1, s1)
      s2
    }
  }

  def apply[A1, A2, A3, S](p1: Rel[A1, S], p2: Rel[A2, S], p3: Rel[A3, S]) = new Rel[(A1, A2, A3), S]() {
    override def toString = "(" + p1 + ", " + p2 + ", " + p3 + ")"

    def parse(s0: S) = {
      val (a1, s1) = p1 parse s0
      val (a2, s2) = p2 parse s1
      val (a3, s3) = p3 parse s2
      ((a1, a2, a3), s3)
    }

    def format[B >: (A1, A2, A3)](bs: (B, S)) = {
      val ((a1, a2, a3), s0) = bs
      val s1 = p3 format (a3, s0)
      val s2 = p2 format (a2, s1)
      val s3 = p1 format (a1, s2)
      s3
    }
  }

  def apply[A1, A2, A3, A4, S](p1: Rel[A1, S], p2: Rel[A2, S], p3: Rel[A3, S], p4: Rel[A4, S]) = new Rel[(A1, A2, A3, A4), S]() {
    override def toString = "(" + p1 + ", " + p2 + ", " + p3 + ", " + p4 + ")"

    def parse(s0: S) = {
      val (a1, s1) = p1 parse s0
      val (a2, s2) = p2 parse s1
      val (a3, s3) = p3 parse s2
      val (a4, s4) = p4 parse s3
      ((a1, a2, a3, a4), s4)
    }

    def format[B >: (A1, A2, A3, A4)](bs: (B, S)) = {
      val ((a1, a2, a3, a4), s0) = bs
      val s1 = p4 format (a4, s0)
      val s2 = p3 format (a3, s1)
      val s3 = p2 format (a2, s2)
      val s4 = p1 format (a1, s3)
      s4
    }
  }
}