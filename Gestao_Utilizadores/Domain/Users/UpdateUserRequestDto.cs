﻿using System;
namespace UserManagement.Domain.Users
{
  public class UpdateUserRequestDto
  {
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public bool Active { get; set; }
  }
}
