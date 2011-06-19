class Scenario
  attr_reader :classes, :students

  def initialize(c, s, p = nil)
    @classes = c
    @students = s
    @pairings = p
    @pairings = [] if !@pairings
    
    remove_unfinalized_pairings!
    fill_in_possible_pairings!
  end

  def pairings
    @pairings.clone
  end

  def rating
    rating = 0

    @classes.each do |c|
      rating = rating.next if paired_classes.include? c
    end

    rating
  end

  def paired_classes
    pairings.map { |p| p.chem_class if p.finalized? }.compact
  end

  def ==(s)
    return false if !s.is_a? self.class
    self.classes.sort == s.classes.sort && self.students.sort == s.students.sort && self.pairings.sort == s.pairings.sort
  end

  private

  def fill_in_possible_pairings!
    @students.each do |student|      
      @pairings.concat(available_classes_for_student(student).map { |c| Pairing.new(student, c) })
    end
  end

  def available_classes_for_student(student)
    @classes.map do |c| 
      c if !c.meeting_times.map do |mt| 
        student.has_time_open?(self, mt) && student.can_accept_responsibility?(self, c)
      end.include?(false) && !paired_classes.include?(c)
    end.compact
  end

  def remove_unfinalized_pairings!
    @pairings = @pairings.find_all { |p| p.finalized? }
  end
end
