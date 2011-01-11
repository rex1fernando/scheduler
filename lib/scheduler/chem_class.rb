class ChemClass
  attr_reader :name, :type, :meeting_times

  @possible_types = [:recitation, :lab]
  @possible_types_abbreviations = { 'R' => :recitation, 'L' => :lab }
  class << self; attr_reader :possible_types, :possible_types_abbreviations; end

  def initialize(n, t, mt)
    @name = n

    @type = ChemClass.possible_types_abbreviations[t] if t.is_a? String
    @type = t if t.is_a? Symbol

    @meeting_times = mt
    
    if @meeting_times[0].is_a? String
      @meeting_times.each_index do |i|
        @meeting_times[i] = TimeInterval.new @meeting_times[i]
      end
    end

    #raise ArgumentError.new("Invalid class type.") unless ChemClass.possible_types.include?(t)
  end

  def tas(scenario)
    scenario.pairings.map {|p| p.student if p.chem_class == self }.compact
  end

  def to_s
    "Class: #{self.name}, Type: #{self.type.to_s}, Meeting Times: #{self.meeting_times.inject { |sum, mt| "#{sum} #{mt.to_s}," }}".gsub(':0', ':00') + "\n"
  end

  def ==(c)
    self.name == c.name && self.type == c.type && self.meeting_times == c.meeting_times
  end

  def <=>(c)
    self.name <=> c.name
  end
end
