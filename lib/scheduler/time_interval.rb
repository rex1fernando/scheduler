class TimeInterval
  attr_reader :start_time, :end_time
  
  def initialize(st, et = nil)
    if st.is_a? String
      init_from_string st
    else
      @start_time = st
      @end_time = et
    end
  end

  
  def intersects_with?(ti)
    @end_time >= ti.start_time && @start_time <= ti.end_time
  end

  def to_s
    "#{@start_time.to_s} - #{@end_time.to_s}"
  end

  def ==(ti)
      @start_time.to_mpsm == ti.start_time.to_mpsm && @end_time.to_mpsm == ti.end_time.to_mpsm
  end

  private
  
  def init_from_string(s)
    without_spaces = s.gsub " ", ""
    weekday_letter = without_spaces[0,1]
    time_strings = without_spaces[1..-1].split "-"
    
    @start_time = WeekTime.new weekday_letter, time_strings[0]
    @end_time = WeekTime.new weekday_letter, time_strings[1]
  end
end
