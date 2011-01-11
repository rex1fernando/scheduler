class Student
  attr_reader :name, :type, :busy_times_without_classes

  @possible_types = [:quarter_time, :half_time]
  class << self; attr_reader :possible_types; end


  def initialize(n, t, bt)
    @name = n

    @type = t if t.is_a? Symbol
    @type = Student.possible_types[t*4 - 1] if t.is_a? Float

    @busy_times_without_classes = bt
    
    if @busy_times_without_classes[0].is_a? String
      @busy_times_without_classes.each_index do |i|
        @busy_times_without_classes[i] = TimeInterval.new @busy_times_without_classes[i]
      end
    end

    raise ArgumentError.new("Invalid student type.") unless Student.possible_types.include? @type
  end

  def classes(scenario)
    scenario.pairings.map {|p| p.chem_class if p.student == self && p.finalized? }.compact
  end

  def busy_times(scenario)
    bt = busy_times_without_classes.clone
    self.classes(scenario).each {|c| bt = bt.concat(c.meeting_times) } 
    bt
  end

  def has_time_open?(scenario, t)
    !self.busy_times(scenario).map { |interval| interval.intersects_with? t }.include? true
  end

  def can_accept_responsibility?(scenario, c)
    if self.type == :half_time
      self.classes(scenario).map { |c2| c2 if c2.type == c.type }.compact.size < 2
    elsif self.type == :quarter_time
      self.classes(scenario).map { |c2| c2 if c2.type == c.type }.compact.size < 1
    end
  end
  
  def to_s
    "Student: #{self.name}, Type: #{self.type.to_s}, Busy Times: #{self.busy_times_without_classes.inject { |sum, mt| "#{sum} #{mt.to_s}," }}".gsub(':0', ':00') + "\n"
  end

  def ==(s)
    self.name == s.name && self.type == s.type && self.busy_times_without_classes == s.busy_times_without_classes
  end

  def <=>(s)
    self.name <=> s.name
  end
end
