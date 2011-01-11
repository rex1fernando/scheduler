class Pairing
  attr_reader :student, :chem_class

  def initialize(s, c, f = false)
    @student = s
    @chem_class = c
    @finalized = f
  end

  def finalize
    Pairing.new @student, @chem_class, true
  end

  def finalized?
    @finalized
  end

  def ==(p)
    self.student == p.student && self.chem_class == p.chem_class
  end

  def <=>(c)
    if (self.student <=> c.student) == 0
      self.chem_class <=> c.chem_class
    else
      self.student <=> c.student
    end
  end
end
