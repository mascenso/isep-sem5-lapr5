﻿namespace UserManagement.Domain.Users
{
  public class UserLoginRequestDto
  {
   public string Email { get; set; }
   public string Password { get; set; }
   public bool Active { get; set; }
  }

}

