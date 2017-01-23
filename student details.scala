/**
Assignment1
*/

case class Student(studentid:Long,studentName:String)		// case class
case class Marks(subjectid:Long,studentid:Long,marksObtained:Float) // case  class 

class Methods 
{
// method for 1st part 
 
def display(markslist:List[Marks],subId:Long,percentage:Float,status:String,index:Int,count:Int):String=
{
status match {
case "pass" =>   if(index<=markslist.length-1)
		{if(markslist(index).subjectid==subId)
		{ if(markslist(index).marksObtained>=percentage) display(markslist,subId,percentage,status,index+1,count+1)
                 else display(markslist,subId,percentage,status,index+1,count)}
		else display(markslist,subId,percentage,status,index+1,count)} else s"pass=$count"


case "fail" => if(index<=markslist.length-1)
		{if(markslist(index).subjectid==subId)
		{ if(markslist(index).marksObtained<percentage) display(markslist,subId,percentage,status,index+1,count+1)
                 else display(markslist,subId,percentage,status,index+1,count)}
		else display(markslist,subId,percentage,status,index+1,count)} else s"fail=$count"

	    }



}

// method for second part

def toppersAndBlowers(markslist:List[Marks],studentlist:List[Student],status:String,subId:Long,index:Int,count:Int):Unit=
{
  status match 
  {
    case "top"=>
      {
	if(count>0)
	{
		if(index==0) {val newlist=markslist.filter(_.subjectid==subId )
  			toppersAndBlowers(newlist,studentlist,status,subId,index+1,count)}
		else
		{
			val highest=markslist.reduce((a,b)=>if (a.marksObtained>=b.marksObtained) a else b)
			studentlist.map(x=>if(x.studentid==highest.studentid) println(x.studentName+" "+highest.marksObtained))
			val reducelist=markslist.filter(_ !=highest)
			toppersAndBlowers(reducelist,studentlist,status,subId,index+1,count-1)
		}
	}	

       }
      
      
      
    case "bottam" => if(count>0)
	{
		if(index==0) {val newlist=markslist.filter(_.subjectid==subId )
		  toppersAndBlowers(newlist,studentlist,status,subId,index+1,count)}
		else
		{
			val highest=markslist.reduce((a,b)=>if (a.marksObtained<b.marksObtained) a else b)
			studentlist.map(x=>if(x.studentid==highest.studentid) println(x.studentName+" "+highest.marksObtained))
			val reducelist=markslist.filter(_ !=highest) // delete highest  marks student object from list and update by tail 				recursion
			toppersAndBlowers(reducelist,studentlist,status,subId,index+1,count-1)

		}
	}
      
    }

  }
}

object StudentDetails extends App {
  
  val studentlist=List(Student(1,"shubham"),Student(2,"charmy"),Student(3,"osho"))
  val markslist=List(Marks(1,1,60),Marks(1,2,80),Marks(1,3,20),
                     Marks(2,1,40),Marks(2,2,90),Marks(2,3,50),
                     Marks(3,1,50),Marks(3,2,30),Marks(3,3,80) )
   val obj1 = new Methods
   println(obj1.display(markslist,1,40,"pass",0,0)) // here 0 and last 0 use for updating list by tail recursion
 
 

obj1.toppersAndBlowers(markslist,studentlist,"bottam",1,0,3) // here 3 define how many student you wants to see from top and from bottam
							     // 0 define index  

 
    
}
