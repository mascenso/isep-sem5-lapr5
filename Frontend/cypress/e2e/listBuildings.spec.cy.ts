describe('Teste da Tabela da lista de edificios', () => {
  
    beforeEach(() => {
        cy.visit('http://localhost:4200/login');
        cy.get('#role').select('Administrador');
        cy.get('#signUp').click();
        cy.get('[id^="Campus"]').click()
    });
  
    
    it('Deve exibir a tabela corretamente e os valores da primeira coluna que vai buscar a bd', () => {
        cy.get('[routerlink="list-buildings"]').click();

        cy.get('table.mat-elevation-z8').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-code').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-name').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-description').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-maxLength').should('exist');
        cy.get('.mat-mdc-header-row > .cdk-column-maxWidth').should('exist');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-name').invoke('text').should('eq', ' Edificio A ');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-description').invoke('text').should('eq', ' Edificio A - Administracao ');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-maxLength').invoke('text').should('eq', ' 500 ');
        cy.get('.mdc-data-table__content > :nth-child(1) > .cdk-column-maxWidth').invoke('text').should('eq', ' 10 ');
    });
  
  });