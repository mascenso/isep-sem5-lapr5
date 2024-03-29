﻿using System;
using Microsoft.EntityFrameworkCore.Migrations;
using UserManagement.Domain.Users;

#nullable disable

namespace UserManagement.Migrations
{
    /// <inheritdoc />
    public partial class InitialCreate : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Users",
                columns: table => new
                {
                    UserId = table.Column<string>(type: "text", nullable: false),
                    Email = table.Column<string>(type: "text", nullable: true),
                    Password = table.Column<string>(type: "text", nullable: true),
                    Password_IsHashed = table.Column<bool>(type: "boolean", nullable: true),
                    FirstName = table.Column<string>(type: "text", nullable: true),
                    LastName = table.Column<string>(type: "text", nullable: true),
                    Role = table.Column<string>(type: "text", nullable: false),
                    Active = table.Column<bool>(type: "boolean", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Users", x => x.UserId);
                });

            migrationBuilder.InsertData(
              table: "Users",
              columns: new[] { "UserId", "Email", "Password", "FirstName", "LastName", "Role", "Active" },
              values: new object[]
              {
                Guid.NewGuid().ToString(),          // UserId
                "admin@email.pt",    // Email
                "$2b$10$9JGYmmkpHiiFQoI63xyqyOhTtRJYK/2kDfy8nNsyA0UBeLioxRNVK",      // Password (hashed for security)
                "Manel",                // FirstName
                "Da Maquina",                 // LastName
                UserRole.ADMINISTRATOR.ToString(), // Role
                true                    // Active
              });
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "Users");
        }
    }
}
