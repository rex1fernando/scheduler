require 'lib/scheduler.rb'

def update_progress(i)
  print "\r\e[0K#{i}"
  $stdout.flush
end

def finish_progress
  print "\r\e[0K"
  $stdout.flush
end

puts "Reading classes...\n\n" # read classes

classes = []
progress = 1

CSV.foreach "classes.csv" do |row|
  classes.push(ChemClass.new(row[0], row[1], row[2..-1]))
  update_progress progress
  progress = progress.next
end
finish_progress

#classes.each do |c|
#  puts c
#end

puts "\nDone reading classes.\n\n"

puts "Reading students...\n\n" # read students & schedules

students = []
progress = 1

CSV.foreach "tas.csv" do |row|
  students.push(Student.new(row[0], row[1].to_f, row[2..-1]))
  update_progress progress
  progress = progress.next
end
finish_progress

#students.each do |s|
#  puts s
#end

puts "\nDone reading students.\n\n"



puts "Waiting for jobs."
s = Scheduler.new classes, students, false
