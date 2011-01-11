class WeekDay
  attr_reader :day_number

  @days = [:sunday, :monday, :tuesday, :wednesday, :thursday, :friday, :saturday]
  @day_letters = ['S', 'M', 'T', 'W', 'R', 'F', 'S']
  class << self; attr_reader :days, :day_letters; end


  def initialize(day)
    @day_number = WeekDay.days.index(day) if day.is_a? Symbol
    @day_number = WeekDay.day_letters.index(day) if day.is_a?(String) && day.size == 1
    @day_number = day if day.is_a? Integer
    
    raise ArgumentError.new("Invalid Weekday.") unless @day_number != nil
    raise ArgumentError.new("Invalid Weekday.") if @day_number.is_a?(Integer) && @day_number > 6

  end

  def to_s
    WeekDay.days[@day_number].to_s
  end

  def to_mpsm
    @day_number * 1440
  end

  def ==(wd)
    this.to_mpsm == wd.to_mpsm
  end
end
