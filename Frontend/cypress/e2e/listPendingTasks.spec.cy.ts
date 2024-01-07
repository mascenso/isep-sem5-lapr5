describe('Teste de listar uma tarefas pendentes', () => {
  
    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#login-input-username').type('admin@email.pt');
        cy.get('#login-input-password').type('admin');
        cy.get('button[type="submit"]').click();

        cy.get('[id^="Tasks"]').click();
        cy.get('[routerlink="pending-task-list"]').click();

    });
  

      it('Deve exibir a tabela corretamente e com valores que vai buscar a bd', () => {
    
        cy.get('table.mat-elevation-z8').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-description').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-user-name').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-user-contact').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-type').should('exist');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-description').should('not.be.empty');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-user-name').should('not.be.empty');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-user-contact').should('not.be.empty');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-type').should('not.be.empty');
      });
 

  });